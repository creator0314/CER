% Monte Carlo experiment for "SDPD_quantile.m"

tic

%% Set Monte Carlo design parameters
n   = 100;
T   = 10;
Rz0 = 1;      % true number of factors
Ry0 = 2;      % true number of factors in the y equation
SNR = 1;      % variance of v and e, 1,2,4,9
R   = 100;   % number of Monte Carlo iterations
rng(20230631);

tau = 0.75;
beta_z0  = [1;1];
beta_y0  = [1;1;0.2;0.2];
lambda0  = 0.2;
sigma_v0 = sqrt(16/12*SNR);
sigma_e0 = sqrt(16/12*SNR);
psi0     = 0.2;

%% Parameters
sigma_xi0 = sqrt((1-psi0^2)*sigma_v0^2);
alpha_xi0 = 1/sigma_xi0;
alpha0 = 1/sigma_e0;
delta0 = psi0*sigma_v0/sigma_e0;
m      = min(n,T);

%% Generate Data
NT      = n*T;
Gamma_y = rand(n,Ry0)*4-ones(n,Ry0)*2;
Gamma_z = rand(n,Rz0)*4-ones(n,Rz0)*2;
F_y     = rand(T,Ry0)*4-ones(T,Ry0)*2;
F_z     = F_y(:,1:Rz0);

Xz = zeros(2,n,T);
Xz(1,:,:) = rand(n,T)*4-ones(n,T)*2 + (Gamma_z*F_z' + Gamma_z*ones(Rz0,1)*ones(1,T) + ones(n,1)*(F_z*ones(Rz0,1))')/3;
Xz(2,:,:) = rand(n,T)*4-ones(n,T)*2;
Xy = zeros(2,n,T);
Xy(1,:,:) = rand(n,T)*4-ones(n,T)*2 ;
Xy(2,:,:) = rand(n,T)*4-ones(n,T)*2;
mu      = [0,0];
Sigma   = [sigma_v0^2,psi0*sigma_v0*sigma_e0;psi0*sigma_v0*sigma_e0,sigma_e0^2];

%% Create the spatial weights matrix from rook adjacency matrix
Wd  = zeros(n,n);
rr  = sqrt(n);
for i = 1:n
    for j = 1:n
        row_i       = ceil(i/rr);
        column_i    = i-(row_i-1)*rr;
        row_j       = ceil(j/rr);
        column_j    = j-(row_j-1)*rr;
        if abs(row_i-row_j)+abs(column_i-column_j) == 1
            Wd(i,j) = 1;
        end
    end
end

%% Monte Carlo Iterations
theta = zeros(10,R);
theta_se = zeros(10,R);
theta_true = zeros(6,R);

for mc_r = 1:R
    V   = zeros(n,T);
    E   = zeros(n,T);
    Y   = zeros(n,T);
    W   = zeros(n,n,T);
    
    Error = mvnrnd(mu,Sigma,NT);
    for t = 1:T
        V(:,t) = Error(1+(t-1)*n:t*n,1);
        E(:,t) = Error(1+(t-1)*n:t*n,2);
    end
    Z = squeeze(Xz(1,:,:))*beta_z0(1) + squeeze(Xz(2,:,:))*beta_z0(2) + Gamma_z*F_z' + E;    
    
    for t = 1:T
        for i = 1:n
            for j = 1:n
                W(i,j,t) = Wd(i,j)*min(1/abs(Z(i,t)-Z(j,t)),2);
            end
        end
        W(:,:,t) = row_normalize(W(:,:,t));
    end
    
    for t = 1:T
        S0 = eye(n)-lambda0*W(:,:,t);
        if t==1
             Y(:,t) = S0\(Xy(1,:,t)'.*(beta_y0(1)) + Xy(2,:,t)'.*(beta_y0(2)) + Gamma_y*F_y(t,:)' + V(:,t));
        else
             Y(:,t) = S0\(Xy(1,:,t)'.*(beta_y0(1)) + Xy(2,:,t)'.*(beta_y0(2))+Y(:,t-1).*(beta_y0(3))+(W(:,:,t)*Y(:,t-1)).*(beta_y0(4))+Gamma_y*F_y(t,:)'+ V(:,t));
         end
    end
    
    fprintf('Monte Carlo Iterations \t round %.0f \t %.2f%% \t \n',mc_r,(mc_r/R)*100);
    %[beta,bcorr,Vbeta,gamma_z,f_z,gamma_y,f_y,exitflag] = esfactors1(Y,W,Xy,Xz,Z,Ry0,Rz0,1e-9);
    [beta,gamma_z,f_z,gamma_y,f_y,exitflag] = Panel_SDPD_quantile1(Y,W,Xy,Xz,Z,Ry0,Rz0,tau,1e-10);
    theta(:,mc_r) = beta;

    Zhat = squeeze(Xz(1,:,:)).*beta(1) + squeeze(Xz(2,:,:))*beta(2) + gamma_z*f_z';    
    Yhat = zeros(n,T);
    for t = 1:T
        if t==1
             Yhat(:,t) = Xy(1,:,t)'.*(beta(5)) + Xy(2,:,t)'.*beta(6) + +(W(:,:,t)*Y(:,t)).*(beta(9)) + gamma_y*f_y(t,:)';
        else
             Yhat(:,t) = Xy(1,:,t)'.*(beta(5)) + Xy(2,:,t)'.*beta(6)+Y(:,t-1).*beta(7)+(W(:,:,t)*Y(:,t-1)).*(beta(8))+(W(:,:,t)*Y(:,t)).*(beta(9))+gamma_y*f_y(t,:)';
        end
    end
    residz = Z - Zhat;
    residy = Y - Yhat;
    theta_se(:,mc_r) = std(bootstrp(10,@(bootry,bootrz)Panel_SDPD_quantile1(Yhat+bootry,W,Xy,Xz,Zhat+bootrz,Ry0,Rz0,tau,1e-10),residy,residz));
    
    Z = squeeze(Xz(1,:,:))*beta_z0(1) + squeeze(Xz(2,:,:))*beta_z0(2) + E;   
    for t = 1:T
        S0 = eye(n)-lambda0*W(:,:,t);
        if t==1
           Y(:,t) = S0\(Xy(1,:,t)'.*(beta_y0(1)) + Xy(2,:,t)'.*(beta_y0(2)) + V(:,t));
        else
           Y(:,t) = S0\(Xy(1,:,t)'.*(beta_y0(1)) + Xy(2,:,t)'.*(beta_y0(2))+Y(:,t-1).*(beta_y0(3))+(W(:,:,t)*Y(:,t-1)).*(beta_y0(4))+V(:,t));
        end
    end
    beta_true = qpsar(Y,W,Xy,Xz,Z,tau,1e-10);
    theta_true(:,mc_r) = beta_true;
end

%%
fprintf('\n Simulation results: \t n = %.0f T = %.0f tau = %.2f \n',n,T,tau);
fprintf('\t beta1 \t true = %.5f \t est = %.5f \t bias = %.5f \t sd = %.5f\t mse = %.5f\n' , mean(theta_true(1,:)) , mean(theta(5,:)) , mean(theta(5,:))-mean(theta_true(1,:)) , mean(theta_se(5,:)) , (mean(theta(5,:))-mean(theta_true(1,:))).^2+(mean(theta_se(5,:))).^2);
fprintf('\t beta2 \t true = %.5f \t est = %.5f \t bias = %.5f \t sd = %.5f\t mse = %.5f\n' , mean(theta_true(2,:)) , mean(theta(6,:)) , mean(theta(6,:))-mean(theta_true(2,:)) , mean(theta_se(6,:)) , (mean(theta(6,:))-mean(theta_true(2,:))).^2+(mean(theta_se(6,:))).^2);
fprintf('\t gamma \t true = %.5f \t est = %.5f \t bias = %.5f \t sd = %.5f\t mse = %.5f\n' , mean(theta_true(3,:)) , mean(theta(7,:)) , mean(theta(7,:))-mean(theta_true(3,:)) , mean(theta_se(7,:)) , (mean(theta(7,:))-mean(theta_true(3,:))).^2+(mean(theta_se(7,:))).^2);
fprintf('\t rho   \t true = %.5f \t est = %.5f \t bias = %.5f \t sd = %.5f\t mse = %.5f\n' , mean(theta_true(4,:)) , mean(theta(8,:)) , mean(theta(8,:))-mean(theta_true(4,:)) , mean(theta_se(8,:)) , (mean(theta(8,:))-mean(theta_true(4,:))).^2+(mean(theta_se(8,:))).^2);
fprintf('\t lambda\t true = %.5f \t est = %.5f \t bias = %.5f \t sd = %.5f\t mse = %.5f\n' , mean(theta_true(5,:)) , mean(theta(9,:)) , mean(theta(9,:))-mean(theta_true(5,:)) , mean(theta_se(9,:)) , (mean(theta(9,:))-mean(theta_true(5,:))).^2+(mean(theta_se(9,:))).^2);
fprintf('\t alpha \t true = %.5f \t est = %.5f \t bias = %.5f \t sd = %.5f\t mse = %.5f\n' , mean(theta_true(6,:)) , mean(theta(10,:)) , mean(theta(10,:))-mean(theta_true(6,:)) , mean(theta_se(10,:)) , (mean(theta(10,:))-mean(theta_true(6,:))).^2+(mean(theta_se(10,:))).^2);

function W = row_normalize(W0)

n = length(W0(:,1));
W = zeros(n,n);

W_sum = zeros(n,1);
for i = 1:n
    for j = 1:n
        W_sum(i) = W_sum(i) + W0(i,j);
    end
end

for i = 1:n
    if W_sum(i) ~= 0
        for j = 1:n
            W(i,j) = W0(i,j)/W_sum(i);
        end
    end
end
end