
A= spconvert(load('RT.txt'));
if size(A,1) ~= 73264 || size(A,2) ~= 73264
    A(73264,73264) = 0;
end

B = spconvert(load('Reponse.txt'));
if size(B,1) ~= 73264 || size(B,2) ~= 73264
    B(73264,73264) = 0;
end

C = spconvert(load('Mention.txt'));
if size(C,1) ~= 73264 || size(C,2) ~= 73264
    C(73264,73264) = 0;
end

whos

% Order multiplex layers in a cell
Mat=cell(1,3);

Mat{1}=A;
Mat{2}=B;
Mat{3}=C;

% multiplex PageRank 
[x,X]=multiplexPageRank(Mat,0.85,1,1);

%classement

[MPR,Ids] = sort(X{3},'descend') %MPR : classement du MPR ; Ids : classement des identifiants

% figure

figure;
plot(X{3},'-ob');

