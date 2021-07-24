program VECTORES_ALEATORIOS

real,parameter ::Pi=3.141592_4
integer ::i,k,p
real, dimension(100000) ::threetups
threetups=0
open(12,file='vector.txt')
do  i = 1, 100000 
	call random_three_vector(vector)
	u=vector
	threetups(i)=u
	write(12,*) threetups(i)
end do
end program

!-------------------------------------------------------
	SUBROUTINE random_three_vector(vector)
		REAL(8) ::x, y, z
		REAL(8) ::theta, phi, costheta
		call RANDOM_NUMBER(r)
                ran1=2.0*r - 1.0
                call RANDOM_NUMBER(r)
		ran2=2*Pi*r-Pi
		phi = ran2
		costheta = ran1
		theta = ACOS( costheta )
		x = sin( theta) * cos( phi )
		y = sin( theta) * sin( phi )
		z = cos( theta )
		vector=x
    	END SUBROUTINE

