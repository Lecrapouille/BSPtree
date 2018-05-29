s=poly(0,'s');
slin=(s+1)/(s^2+5*s+1-0.0*s^5);
Plant=syslin('c',slin);

t=0:0.1:50;
a=csim('step',t,Plant);
plot(a);

//poles et zeros
plzr(Plant);

//
// PID
//
// k: gain
// Ti: integrale
// Td: derivee
k=10; Ti=0.1; Td=0;

// controleur
con=k+1/(Ti*s)+s*Td;
//boucle fermee
bf=con*slin/(1+con*slin);
bflin=syslin('c',bf);
aa=csim('step',t,bflin);
plot(aa);

//nyquist
h=syslin('c',con*slin);
nyquist(h, 0.03,10);

===================================
s=poly(0,'s');

slin=5*(s-0.1)/((s-1)*(s-0.5));
fb=slin/(1+slin);
fblin=syslin('c',fb);
t=0:0.1:50;
h=syslin('c',slin);
nyquist(h, -10,10);

aa=csim('step',t,h);
plot(aa);

plzr(fb);