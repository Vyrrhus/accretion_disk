module module_integration
use input

contains

!---------------------------------------------------------------

function euler(f,y,t,dt) result(y_new)
implicit none
real(kind=dp),intent(in)::dt,y(ndim),t
real(kind=dp)::y_new(ndim)
interface
       function f(y,t)
         implicit none
         integer, parameter :: dp = selected_real_kind(15)
	 integer, parameter :: ndim=2
         real(kind=dp)::f(ndim)
         real(kind=dp),intent(in)::y(ndim),t
       end function f
      end interface
y_new = y + dt*f(y,t)
end function euler

!---------------------------------------------------------------

function rk2(f,y,t,dt) result(y_new)
implicit none
real(kind=dp),intent(in)::dt,y(ndim),t
real(kind=dp)::y_new(ndim),k_1(ndim),k_2(ndim)
interface
       function f(y,t)
         implicit none
         integer, parameter :: dp = selected_real_kind(15)
         integer, parameter :: ndim=2
         real(kind=dp)::f(ndim)
         real(kind=dp),intent(in)::y(ndim),t
       end function f
      end interface
k_1 = f(y,t)*dt
k_2 = f(y+k_1/2.0_dp,t+dt/2.0_dp)*dt
y_new = y + k_2
end function rk2

!---------------------------------------------------------------

function rk4(f,y,t,dt) result(y_new)
implicit none
real(kind=dp),intent(in)::dt,y(ndim),t
real(kind=dp)::y_new(ndim),k_1(ndim),k_2(ndim),k_3(ndim),k_4(ndim)
interface
       function f(y,t)
         implicit none
         integer, parameter :: dp = selected_real_kind(15)
         integer, parameter :: ndim=2
         real(kind=dp)::f(ndim)
         real(kind=dp),intent(in)::y(ndim),t
       end function f
      end interface
      
k_1 = f(y,t)*dt
k_2 = f(y+k_1/2.0_dp,t+dt/2.0_dp)*dt
k_3 = f(y+k_2/2.0_dp,t+dt/2.0_dp)*dt
k_4 = f(y+k_3,t+dt)*dt
y_new = y + (k_1+2.0_dp*k_2+2.0_dp*k_3+k_4)/6.0_dp

end function rk4

!----------------------------------------------------------------

function logspace(v_i,v_f,nb) result(res)
IMPLICIT NONE
real(kind=dp) , intent(in) :: v_i,v_f
integer , intent(in) :: nb
real(kind=dp):: res(nb)
integer :: i

do i=1,nb
	res(i) = 10.0_dp**(v_i+(v_f-v_i)*(i-1.0_dp)/(nb-1.0_dp))
enddo
end function logspace

end module stuff

!---------------------------------------------------------------
!---------------------------------------------------------------

module integration_diff
use stuff
implicit none

contains

function integration(filename,which,f,y_0,dt,t_0,t_max,cond,erreur,pas_ecriture) result(t_lim)
implicit none 

real(kind=dp),intent(in)::y_0(ndim),dt,t_0,t_max,erreur
character(len=1024),intent(in)::filename
integer,intent(in)::cond,pas_ecriture
integer::i
real(kind=dp)::y_1(ndim),told,t
real(kind=dp)::t_lim

real(kind=dp)::u,e_g,M

interface 
       function f(y,t)
         implicit none
         integer, parameter :: dp = selected_real_kind(15)
         integer, parameter :: ndim=2
         real(kind=dp)::f(ndim)
         real(kind=dp),intent(in)::y(ndim),t
       end function f
end interface

interface 
	function which(f,y,t,dt)
	use stuff
	implicit none
	real(kind=dp)::which(ndim)
	real(kind=dp),intent(in)::y(ndim),t,dt
	interface 
         function f(y,t)
         implicit none
         integer, parameter :: dp = selected_real_kind(15)
         integer, parameter :: ndim=2
         real(kind=dp)::f(ndim)
         real(kind=dp),intent(in)::y(ndim),t
         end function f
	end interface
	end function which
end interface

open(11,file=trim(filename),status='unknown')
if (cond==1) then 
	y_1=y_0
	t=t_0
	do while(t<=t_max)
		y_1=which(f,y_1,t,dt)
		t=t+dt
		write(11,*)t,y_1(1),y_1(2)
	enddo
	t_lim=t
endif

if (cond==2) then
	y_1=y_0
	t=t_0
	i = 1
	do while(t<=t_max)
		y_1=which(f,y_1,t,dt)
		t=t+dt
		if (mod(i,pas_ecriture)==0) then
		write(11,*)t,y_1(1),y_1(2)
		endif
		i=i+1
	enddo
	t_lim=t
endif

if (cond==3) then
	y_1=y_0
	t=t_0
	do while(y_1(1)>erreur)
		told=t
		y_1=which(f,y_1,t,dt)
		t=t+dt
		write(11,*)t,y_1(1),y_1(2)
		i=i+1
	enddo
	t_lim=told
endif

if (cond==4) then
	y_1=y_0
	t=t_0
	i=1
	M = 4/3*pi*t_0**3*r_deg**3*y_0(1)**3*K1
	do while(y_1(1)>erreur)
		told=t
		y_1=which(f,y_1,t,dt)
		t=t+dt
		M = M + 4*pi*t**2*y_1(1)**3*K1*r_deg**3*dt
		if (mod(i,pas_ecriture)==0) then
		write(11,*)t,y_1(1),y_1(2)
		endif
		i=i+1
	enddo
	t_lim=told*r_deg/R_o
	write(*,*)y_0(1)**3*K1/1e15,t_lim,M/M_o
endif
 close(11)
end function integration

!-------------------------------------------------------------------

end module module_integration
