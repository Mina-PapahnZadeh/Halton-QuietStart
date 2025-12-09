	implicit double precision(a-h,o-z)
	double precision, dimension (1:200000)::vmt,ratmt
	open(5,file="random_eqt2.dat", status="unknown")
	
	do i = 1, 200000
	read(5,*) vmt(i),ratmt(i)
	enddo
	close(5)

	xuw = 0.006
	xlw = 0.00	

	Nc=204800000

	open(1,file="ion_phase.dat",status="unknown")
	
	do j = 1,Nc	
	
	call halton2(j,res)

c this is just a random value	
	ermin = 45.00
	Rs = res
	do i = 1,200000
	
	er = ratmt(i)-Rs
	aer = abs(er)
	if (aer.le.ermin) then
	vsol = vmt(i)

	ermin = aer
	
	endif
	enddo

	call halton1(j,res2)
	xntht = res2

	x = (xuw-xlw)*xntht
	
	write(1,*)x,vsol
	
	enddo
	
	
	close(1)
	


	
	
	

	

	
	


	

	
	stop
	end

	

	subroutine halton1(ind,res)
	implicit double precision (a-h,o-z)
	
	res = 0.0
	ibase = 2
	
	f = 1.0/ibase
	i = ind
	do while((i).gt.(0))
	ia = mod(i,ibase)
	res = res + f*ia
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

	
	
