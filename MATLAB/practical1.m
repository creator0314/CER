%% 
%data 
%used for practical 
N=334;
T=6;
Ry0=2;
Rz0=2;
nss=334;
R=1;
tau=0.5;
for n=0:(N-1)
    Y(n+1,1:(T-1))=log(fproperty(n*T+2:n*T+6));% 家庭净收入
end
Z=Y;
for n=0:(N-1)
    X(1,n+1,1:(T-1))=log(index_aggregate(n*T+2:n*T+6));% 数字普惠金融指数
end
for n=0:(N-1)
    X(2,n+1,1:(T-1))=age(n*T+2:n*T+6);%户主年龄
end
for n=0:(N-1)
    X(3,n+1,1:(T-1))=log(bank_debit(n*T+2:n*T+6)+1);% 银行贷款
end
for n=0:(N-1)
    X(4,n+1,1:(T-1))=child_ratio(n*T+2:n*T+6);
end
for n=0:(N-1)
    X(5,n+1,1:(T-1))=company(n*T+2:n*T+6); %有无参与私营公司
end
for n=0:(N-1)
    X(6,n+1,1:(T-1))=edu1(n*T+2:n*T+6);%户主受教育水平
end
for n=0:(N-1)
    X(7,n+1,1:(T-1))=edu2(n*T+2:n*T+6);
end
for n=0:(N-1)
    X(8,n+1,1:(T-1))=edu3(n*T+2:n*T+6);
end
for n=0:(N-1)
    X(9,n+1,1:(T-1))=edu4(n*T+2:n*T+6);
end
for n=0:(N-1)
    X(10,n+1,1:(T-1))=edu5(n*T+2:n*T+6);
end
for n=0:(N-1)
    X(11,n+1,1:(T-1))=familysize(n*T+2:n*T+6);%家庭规模
end
for n=0:(N-1)
    X(12,n+1,1:(T-1))=gender(n*T+2:n*T+6);%户主性别
end
for n=0:(N-1)
    X(13,n+1,1:(T-1))=hea1(n*T+2:n*T+6);%健康状态
end
for n=0:(N-1)
    X(14,n+1,1:(T-1))=hea2(n*T+2:n*T+6);
end
for n=0:(N-1)
    X(15,n+1,1:(T-1))=hea3(n*T+2:n*T+6);
end
for n=0:(N-1)
    X(16,n+1,1:(T-1))=hea4(n*T+2:n*T+6);
end
for n=0:(N-1)
    X(17,n+1,1:(T-1))=job(n*T+2:n*T+6);%有无工作
end
for n=0:(N-1)
    X(18,n+1,1:(T-1))=log(marketvalue(n*T+2:n*T+6)+1);%金融资产价值
end
for n=0:(N-1)
    X(19,n+1,1:(T-1))=marry(n*T+2:n*T+6);%婚姻状态
end
for n=0:(N-1)
    X(20,n+1,1:(T-1))=old_ratio(n*T+2:n*T+6);%老人占比
end
for n=0:(N-1)
    X(21,n+1,1:(T-1))=party(n*T+2:n*T+6);%有无政党
end
for n=0:(N-1)
    X(22,n+1,1:(T-1))=phone(n*T+2:n*T+6);%有无手机
end
for n=0:(N-1)
    X(23,n+1,1:(T-1))=log(saving(n*T+2:n*T+6)+1);%存款
end
for n=0:(N-1)
    X(24,n+1,1:(T-1))=log(trco(n*T+2:n*T+6)+1);%通讯费指出
end
for n=0:(N-1)
    X(25,n+1,1:(T-1))=sum_fproperty(n*T+2:n*T+6);%省内均值
end
for n=0:(N-1)
    X(26,n+1,1:(T-1))=sum_fproperty_lag(n*T+2:n*T+6);%省内均值
end

W(1:N,1:N,1)=matrixfpropertyeco2;
%W(:,:,1)=row_normalize(matrix_geo2);%矩阵
W(1:N,1:N,2)=matrixfpropertyeco3;
%W(:,:,2)=row_normalize(matrix_geo3);
W(1:N,1:N,3)=matrixfpropertyeco4;
%W(:,:,3)=row_normalize(matrix_geo4);
W(1:N,1:N,4)=matrixfpropertyeco5;
%W(:,:,4)=row_normalize(matrix_geo5);
W(1:N,1:N,5)=matrixfpropertyeco6;
%W(:,:,5)=row_normalize(matrix_geo6);

beta_record = zeros(R,59);
Vbeta_record = zeros(R,60);
f_y_record = zeros(R,Ry0,T-1);
f_z_record = zeros(R,Rz0,T-1);
%%
for mc_r = 1:R
    opt = optimoptions('fminunc','TolFun',1e-9,'TolX',1e-9,'MaxFunEvals',100000,'MaxIter',100000,'Display','notify','Algorithm','quasi-newton');
    fprintf('Monte Carlo Iterations \t round %.0f \t %.2f%% \t \n',mc_r,(mc_r/R)*100);
    ns=sort(datasample(1:N,nss,'replace',false));
    X1=X(:,ns,:);
    Y1=Y(ns,:);
    W1=W(ns,ns,:);
    Z1=Y1;
    [beta,bcorr,Vbeta,gamma_z,f_z,gamma_y,f_y,exitflag] = Panel_SDPD_quantile1(Y1,W1,X1,X1,Z1,Ry0,Rz0,tau,1e-9);
    
    beta_record(mc_r,:) = beta';
    for i = 1:60
        Vbeta_record(mc_r,i) = (Vbeta(i,i)).^(1/2);
    end
    f_y_record(mc_r,:,:) = f_y';
    f_z_record(mc_r,:,:) = f_z';
    %[beta,gamma_z,f_z,gamma_y,f_y,exitflag] =Panel_SDPD_quantile2(W1,X1,Y1,Z1,Ry0,Rz0,tau,1e-1);
    %[beta,bcorr,Vbeta,gamma_z,f_z,gamma_y,f_y,exitflag] = esfactors1(Y1,W1,X1,X1,Z1,Ry0,Rz0,1e-1);
    %record_beta(mc_r,:)=beta';
end

%%
beta=zeros(41,1);
std=zeros(41,1);
for i = 1:41
    beta(i,1)=mean(record_beta(:,i));
end
for i= 1:41
    std(i,1)=var(record_beta(:,i)).^(1/2);
end