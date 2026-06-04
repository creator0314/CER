function [beta,gamma_z,f_z,gamma_y,f_y,exitflag] = Panel_SDPD_quantile1(Y,W,Xy,Xz,Z,Ry,Rz,tau,TolX)%tau
    N = size(Y,1);
    T = size(Y,2);
    Ky = size(Xy,1);
    Kz = size(Xz,1);
    Y_tlag=zeros(N,T);
    Y_tslag=zeros(N,T);
    for i = 1:(T-1)
        Y_tlag(1:N,i+1) = Y(1:N,i);
        Y_tslag(1:N,i+1) = W(:,:,(i+1))*Y(1:N,i);
    end
    Xy(Kz+1,:,:)=Y_tlag;
    Xy(Kz+2,:,:)=Y_tslag;
    Z_tlag=zeros(N,T);
    Z_tslag=zeros(N,T);
    for i = 1:(T-1)
        Z_tlag(1:N,i+1) = Z(1:N,i);
        Z_tslag(1:N,i+1) = W(:,:,(i+1))*Z(1:N,i);
    end
    Xz(Kz+1,:,:)=Z_tlag;
    Xz(Kz+2,:,:)=Z_tslag;
    
    Kz = size(Xz,1);
    Ky = size(Xy,1);
    opt = optimoptions('fminunc','TolX',TolX,'MaxFunEvals',100000,'MaxIter',100000,'Display','notify','Algorithm','quasi-newton','UseParallel',1);
    % try multiple starting values
    fval0 = 1/eps;
    for i = 1:9
        start = randn(Kz+Ky+4,1);
        [est,fval,exitflag0] = fminunc(@Q_obj,start,opt,Y,W,Xy,Xz,Z,Ry,Rz,tau);% tau
        if fval < fval0 && exitflag0 > 0
            fval0 = fval;
            beta = est;
            exitflag = exitflag0;
        end
    end
    
    beta_z  = beta(1:Kz);
    beta_y  = beta(Kz+1:Ky+Kz);
    lambda  = (exp(2*beta(Ky+Kz+1))-1)/(exp(2*beta(Ky+Kz+1))+1);  % lambda < 1, > -1
    alpha_xi= exp(beta(Ky+Kz+2));    % sigma_xi^{-1}
    alpha   = exp(beta(Ky+Kz+3));    % sigma_epsilon^{-1}
    delta   = beta(Ky+Kz+4);
    sigma_e = 1/alpha;   
    sigma_xi    = 1/alpha_xi; 
    
    Xzsum = zeros(N,T);
    for k = 1:Kz
        Xzsum = Xzsum + beta_z(k)*squeeze(Xz(k,:,:));
    end
    Xysum = zeros(N,T);
    for k = 1:Ky
        Xysum = Xysum + beta_y(k)*squeeze(Xy(k,:,:));
    end          
    
    %% Matrix G_tilde (NT x NT)
    GT = zeros(N*T,N*T);
    for t = 1:T
        n0 = 1+(t-1)*N;
        n1 = t*N;
        GT(n0:n1,n0:n1) = W(:,:,t)/(eye(N)-lambda*W(:,:,t));
    end
    
    %% Spatially generated regressors
    X_SAR = zeros(N,T);
    for t = 1:T
        X_SAR(:,t) = W(:,:,t)*Y(:,t);
    end
    
    %% Projection matrices
    %% Gamma_z
    M = (1/sigma_e)*(Z-Xzsum);
    [U,D,~] = svd(M);
    D = diag(D).*diag(D);
    [~,I] = sort(D);
    Gamma_z = zeros(N,Rz);
    for c = 1:Rz
        Gamma_z(:,c) = U(:,I(end-c+1));
    end

    %% Time factors F_z
    [U,D,~] = svd(M');
    D = diag(D).*diag(D);
    [~,I] = sort(D);
    F_z = zeros(T,Rz);
    for c = 1:Rz
        F_z(:,c) = U(:,I(end-c+1));
    end

    %% Gamma_y
    G = zeros(N,T);
    for t = 1:T
        S = eye(N)-lambda*squeeze(W(:,:,t));
        diff = S*Y(:,t)-Xysum(:,t)-(Z(:,t)-Xzsum(:,t))*delta;
        for n = 1:N
            if diff(n,1)>0
              G(n,t) =  (1/sigma_xi)*tau*(diff(n,1));
            else
              G(n,t) = -(1-tau)* (1/sigma_xi)*(diff(n,1));
            end
        end
    end
    
    [U,D,~] = svd(G);
    D = diag(D).*diag(D);
    [~,I] = sort(D);
    Gamma_y = zeros(N,Ry);
    for c = 1:Ry
        Gamma_y(:,c) = U(:,I(end-c+1));
    end

    %% Time factors F_y
    [U,D,~] = svd(G');
    D = diag(D).*diag(D);
    [~,I] = sort(D);
    F_y = zeros(T,Ry);
    for c = 1:Ry
        F_y(:,c) = U(:,I(end-c+1));
    end 
    
    % parameter vector: gamma;rho;beta;lambda;alpha
    beta = [beta_z;beta_y;lambda;alpha];
    
    % factor loadings
    gamma_z = Gamma_z;
    gamma_y = Gamma_y;
    
    % time factors, obtained from cross sectional regressions
    f_y = zeros(T,Ry);
    for t = 1:T
        res = (eye(N)-lambda*squeeze(W(:,:,t)))*Y(:,t)-Xysum(:,t)-(Z(:,t)-Xzsum(:,t))*delta;
        f_y(t,:) = ((gamma_y'*gamma_y)\(gamma_y'*res))'; 
    end
    f_z = ((gamma_z'*gamma_z)\(gamma_z'*(Z-Xzsum)))'; 
return

function f = Q_obj(theta,Y,W,Xy,Xz,Z,Ry,Rz,tau)% tau
    % This function evaluates the likelihood function Q_nT(theta), see
    % Eq.6.
    % Ry(Rz) factors are concentrated out from y(z).
    N = size(Y,1);
    T = size(Y,2);
    Ky = size(Xy,1);
    Kz = size(Xz,1);
   
    beta_z  = theta(1:Kz);
    beta_y  = theta(Kz+1:Ky+Kz);
    lambda  = (exp(2*theta(Ky+Kz+1))-1)/(exp(2*theta(Ky+Kz+1))+1);  % lambda < 1, > -1
    alpha_xi= exp(theta(Ky+Kz+2));    % sigma_xi^{-1}
    alpha   = exp(theta(Ky+Kz+3));    % sigma_epsilon^{-1}
    delta   = theta(Ky+Kz+4);

    sigma_eps_sq = 1/alpha^2;
    sigma_xi_sq  = 1/alpha_xi^2;

    Xzsum = zeros(N,T);
    for k = 1:Kz
        Xzsum = Xzsum + beta_z(k)*squeeze(Xz(k,:,:));
    end
    Xysum = zeros(N,T);
    for k = 1:Ky
        Xysum = Xysum + beta_y(k)*squeeze(Xy(k,:,:));
    end      
    
    ll = -(1/2)*log(sigma_eps_sq)-(1/2)*log(sigma_xi_sq);
    for t = 1:T 
        S = eye(N)-lambda*squeeze(W(:,:,t));
        ll = ll + log(det(S))/(N*T);
    end      
    
    D = (1/sqrt(sigma_eps_sq))*(Z-Xzsum);
 
    G = zeros(N,T);
    for t = 1:T
        S = eye(N)-lambda*squeeze(W(:,:,t));
        diff = S*Y(:,t)-Xysum(:,t)-(Z(:,t)-Xzsum(:,t))*delta;
        for n = 1:N
            if diff(n,1)>0
              G(n,t) = tau*(diff(n,1));
            else
              G(n,t) = -(1-tau)*(diff(n,1));
            end
        end
    end
    
    for t = 1:T
        G(:,t) = (1/sqrt(sigma_xi_sq))*G(:,t);
    end
    
if sum(sum(isnan(D))) ~= 0 || sum(sum(isinf(D))) ~= 0 || sum(sum(isnan(G))) ~= 0 || sum(sum(isinf(G))) ~= 0 || isnan(ll) == 1 || isinf(ll) == 1
    f = 4.503e+15;
else
    [~,D1,~] = svd(D);
    D1 = diag(D1).*diag(D1);
    D1 = sort(D1);

    [~,G1,~] = svd(G);
    G1 = diag(G1).*diag(G1);
    G1 = sort(G1);
    
    %fprintf("%.5f\n",sum(D1(end))/sum(D1(1:end)));
    %fprintf("%.5f\n\n",sum(D1(end-1))/sum(D1(1:end)));

    %fprintf("%.5f\n",sum(G1(end))/sum(G1(1:end)));
    %fprintf("%.5f\n\n",sum(G1(end-1))/sum(G1(1:end)));
    ll = ll-(1/2)*sum(D1(1:end-Rz))/(N*T)-(1/2)*sum(G1(1:end-Ry))/(N*T);
    
    f = abs(ll);
end    