	implicit double precision (a-h,o-z)
	double precision, dimension(0:200000)::vd,fd,xd,fxd
	
	open(3,file="data.dat",status="unknown")
	open(7,file="datax.dat",status="unknown")
	
	eps0 = 8.85418782e-12
	em = 9.10938291e-31
	e = 1.60217657e-19
	pi = 2*asin(1.0)
	

	
	vt1=187553.636125138
	v0 = 2*vt1



	vl = -10.0*vt1+v0
	vu = 10.0*vt1+v0
	delv= (vu-vl)/200000
	
	xu=0.006
	xl=0.0
	delx=(xu-xl)/200000


	do i = 0,200000

	v = i*delv+vl

	a1 = -((v-v0)**2)/(2*(vt1**2))
	a2 = ((2*pi)**0.5)*vt1

	f1 = exp(a1)/a2




	write(3,*) v,f1

	enddo

	close(3)
	
	ae=1e-2
	am=44.0
	al=(xu-xl)/am
	ak=2*pi/al
	
	
	do i  = 0, 200000
	x=i*delx+xl
	f3 = (1.0+ae*cos(ak*x))/(xu-xl)
	write(7,*) x,f3
	enddo
	
	close(7)
	


	open (3,file="data.dat",status="unknown")
	open (7,file="datax.dat",status="unknown")
	do i = 0 , 200000
	read(3,*) v,f
	read(7,*) x,f3
	vd(i) = v
	fd(i) = f
	xd(i)=x
	fxd(i)=f3
	enddo
	close(3)
	close(7)


	ta=0.000
	tax=0.000
	open(4,file="integrate.dat",status="unknown")
	open(8,file="integrate_x.dat",status="unknown")
	do i = 1,200000
	dv = vd(i)- vd(i-1)
	h = (fd(i)+fd(i-1))/2.0
	area = dv*h
	ta = ta + area
	
	dx = xd(i)- xd(i-1)
	hx = (fxd(i)+fxd(i-1))/2.0
	areax = dx*hx
	tax = tax + areax
	write(4,*) vd(i),area,ta
	write(8,*) xd(i),areax,tax
	enddo
	close(4)
	close(8)


	open(4,file="integrate.dat",status="unknown")
	open(5,file="random_eqt.dat",status="unknown")
	open(8,file="integrate_x.dat",status="unknown")
	open(9,file="random_eqt_x.dat",status="unknown")
	do i = 1,200000
	read(4,*) v,area,ca
	rat = ca/ta
	write(5,*) v,rat
	
	read(8,*) x,areax,cax
	ratx = cax/tax
	write(9,*) x,ratx
	enddo
	close(4)
	close(5)
	close(8)
	close(9)

	stop
	end


	
