#define m 128

module vars_mod
  type :: container
     integer, allocatable :: x(:)
  end type container
  type(container) :: a(m), b(m), c(m)

!$acc declare create(a,b,c)
end module vars_mod

module kernel_mod
contains
subroutine kernel(j,n)
use vars_mod, only: a,b,c
implicit none

integer, intent(in) :: j,n
integer :: i
!$acc routine vector
!$acc data present(a,b,c)

!$acc loop vector
do i=1,n
   c(j)%x(i) = a(j)%x(i) + b(j)%x(i)
enddo

!$acc end data
end subroutine kernel
end module kernel_mod

program main
use vars_mod, only: a,b,c
use kernel_mod, only: kernel
implicit none
integer :: i,j,n

n = 32
do j=1,m
   allocate(a(j)%x(n))
   allocate(b(j)%x(n))
   allocate(c(j)%x(n))
   c(j)%x = 0
   do i=1,n
     a(j)%x(i) = i
     b(j)%x(i) = 2*a(j)%x(i)
   enddo
enddo

!$acc update device(a,b,c)

!$acc parallel loop gang
do j=1,m
  call kernel(j,n)
enddo

!$acc update self(c)

do j=1,m
  do i=1,n
    if(c(j)%x(i) /= (a(j)%x(i) + b(j)%x(i)))then
       print *, i, c(j)%x(i), a(j)%x(i), b(j)%x(i)
       error stop
    endif
  enddo

  deallocate(a(j)%x)
  deallocate(b(j)%x)
  deallocate(c(j)%x)
enddo

end program main
