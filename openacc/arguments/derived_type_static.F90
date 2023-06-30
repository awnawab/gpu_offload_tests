#define n 32
#define m 128

module vars_mod
  type :: container
     integer :: x(n)
  end type container
end module vars_mod

module kernel_mod
contains
subroutine kernel(a,b,c)
use vars_mod, only: container
implicit none

type(container), intent(in) :: a,b
type(container), intent(out) :: c
integer :: i
!$acc routine vector
!$acc data present(a,b,c)

!$acc loop vector
do i=1,n
   c%x(i) = a%x(i) + b%x(i)
enddo

!$acc end data
end subroutine kernel
end module kernel_mod

program main
use vars_mod, only: container
use kernel_mod, only: kernel
implicit none
type(container) :: a(m), b(m), c(m)

integer :: i,j

do j=1,m
  c(j)%x = 0
  do i=1,n
    a(j)%x(i) = i + j
    b(j)%x(i) = 2*a(j)%x(i)
  enddo
enddo

!$acc enter data create(c)
!$acc enter data copyin(a,b)

!$acc parallel loop gang
do j=1,m 
   call kernel(a(j), b(j), c(j))
enddo

!$acc exit data delete(a,b)
!$acc exit data copyout(c)

do j=1,m
  do i=1,n
    if(c(j)%x(i) /= (a(j)%x(i) + b(j)%x(i)))then
       print *, i, j, c(j)%x(i), a(j)%x(i), b(j)%x(i)
       error stop
    endif
  enddo
enddo

end program main
