	implicit double precision(a-h,o-z)
	double precision, dimension (1:200000)::vmt,ratmt,xmt,ratxmt
	open(5,file="random_eqt.dat", status="unknown")
	open(6,file="random_eqt_x.dat", status="unknown")
	
	do i = 1, 200000
	read(5,*) vmt(i),ratmt(i)
	read(6,*) xmt(i),ratxmt(i)
	
	enddo
	close(5)
	close(6)

	Nc=20480000

	open(1,file="elec_phase.dat",status="unknown")
	
	
	do j = 1,Nc
	
	
	call halton2(j,res)
	call halton1(j,resx)

c this is just a random value	
	ermin = 45.00
	erminx=45.00
c	res=rand(0)
	Rs = res
	Rsx=resx
	

	do i = 1,200000

	
	er = ratmt(i)-Rs
	aer = abs(er)
	if (aer.le.ermin) then
	vsol = vmt(i)

	ermin = aer
	
	
	endif
	
	erx = ratxmt(i)-Rsx
	aerx = abs(erx)
	
	
	if (aerx.le.erminx) then
	xsol = xmt(i)
	
	

	erminx = aerx
	
	endif
	enddo

	write(1,*)xsol,vsol

	enddo
	
	close(1)
	
	stop
	end

	

	subroutine halton1(ind,resx)
	implicit double precision (a-h,o-z)
	
	resx = 0.0
	ibase = 2
	
	f = 1.0/ibase
	i = ind
	do while((i).gt.(0))
	ia = mod(i,ibase)
	resx = resx + f*ia
	a = (i/ibase)
	i = floor(a)
	f = f/ibase
	enddo
	
	
	end


	subroutine halton2(ind,res2)
	implicit double precision (a-h,o-z)
	
	res2 = 0.0
	ibase = 3
	
	f = 1.0/ibase
	i = ind
	do while((i).gt.(0))
	ia = mod(i,ibase)
	res2 = res2 + f*ia
	a = (i/ibase)
	i = floor(a)
	f = f/ibase
	enddo
	
	
	end

	
	
