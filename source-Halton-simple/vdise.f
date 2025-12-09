	implicit double precision (a-h,o-z)
	double precision, dimension(0:200000)::vd,fd
	
	open(3,file="data.dat",status="unknown")
	
	eps0 = 8.85418782e-12
	em = 9.10938356e-31
	e = 1.60217657e-19
	pi = 2*asin(1.0)
	

	
	vt1=187553.636125138
	v0 = 0.0



	vl = -10.0*vt1+v0
	vu = 10.0*vt1+v0
	delv= (vu-vl)/200000


	do i = 0,200000

	v = i*delv+vl

	a1 = -((v-v0)**2)/(2*(vt1**2))
	a2 = ((2*pi)**0.5)*vt1

	f1 = exp(a1)/a2




	write(3,*) v,f1

	enddo

	close(3)


	open (3,file="data.dat",status="unknown")
	do i = 0 , 200000
	read(3,*) v,f
	vd(i) = v
	fd(i) = f
	enddo
	close(3)


	ta=0.000
	open(4,file="integrate.dat",status="unknown")
	do i = 1,200000
	dv = vd(i)- vd(i-1)
	h = (fd(i)+fd(i-1))/2.0
	area = dv*h
	ta = ta + area
	write(4,*) vd(i),area,ta
	enddo
	close(4)


	open(4,file="integrate.dat",status="unknown")
	open(5,file="random_eqt.dat",status="unknown")
	do i = 1,200000
	read(4,*) v,area,ca
	rat = ca/ta
	write(5,*) v,rat
	enddo
	close(4)
	close(5)

	stop
	end


	
