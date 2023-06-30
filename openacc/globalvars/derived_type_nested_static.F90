#define n 32
#define m 128

module vars_mod
  type :: container
     integer :: x(n)
  end type container

  type :: bucket
     type(container) :: a(m), b(m), c(m)
  end type bucket

type(bucket) :: v
!$acc declare create(v)
end module vars_mod

module kernel_mod
contains
subroutine kernel(j)
use vars_mod, only: v
implicit none

integer, intent(in) :: j
integer :: i
!$acc routine vector
!$acc data present(v)

!$acc loop vector
do i=1,n
   v%c(j)%x(i) = v%a(j)%x(i) + v%b(j)%x(i)
enddo

!$acc end data
end subroutine kernel
end module kernel_mod

program main
use vars_mod, only: v
use kernel_mod, only: kernel
implicit none

integer :: i,j

do j=1,m
  v%c(j)%x = 0
  do i=1,n
    v%a(j)%x(i) = i + j
    v%b(j)%x(i) = 2*v%a(j)%x(i)
  enddo
enddo

!$acc update device(v)

!$acc parallel loop gang
do j=1,m 
   call kernel(j)
enddo

!$acc update self(v)

do j=1,m
  do i=1,n
    if(v%c(j)%x(i) /= (v%a(j)%x(i) + v%b(j)%x(i)))then
       print *, i, j, v%c(j)%x(i), v%a(j)%x(i), v%b(j)%x(i)
       error stop
    endif
  enddo
enddo

end program main
