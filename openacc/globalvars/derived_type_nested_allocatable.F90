module vars_mod
  type :: container
     integer, allocatable :: x(:)
  end type container

  type :: bucket
     type(container), allocatable :: a(:), b(:), c(:)
  end type bucket

  type(bucket) :: v
!$acc declare create(v)
end module vars_mod

module kernel_mod
contains
subroutine kernel(j,n)
use vars_mod, only: v
implicit none

integer, intent(in) :: j,n
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
integer :: i,j,n,m

n = 32
m = 128

allocate(v%a(m))
allocate(v%b(m))
allocate(v%c(m))
do j=1,m
   allocate(v%a(j)%x(n))
   allocate(v%b(j)%x(n))
   allocate(v%c(j)%x(n))
   v%c(j)%x = 0
   do i=1,n
     v%a(j)%x(i) = i
     v%b(j)%x(i) = 2*v%a(j)%x(i)
   enddo
enddo

!$acc enter data create(v%a,v%b,v%c)
do j=1,m
!$acc enter data copyin(v%a(j)%x, v%b(j)%x)
!$acc enter data create(v%c(j)%x)
enddo

!$acc parallel loop gang
do j=1,m
  call kernel(j,n)
enddo

do j=1,m
!$acc exit data delete(v%a(j)%x, v%b(j)%x)
!$acc exit data copyout(v%c(j)%x)
enddo
!$acc exit data delete(v%a,v%b,v%c)

do j=1,m
  do i=1,n
    if(v%c(j)%x(i) /= (v%a(j)%x(i) + v%b(j)%x(i)))then
       print *, i, v%c(j)%x(i), v%a(j)%x(i), v%b(j)%x(i)
       error stop
    endif
  enddo

  deallocate(v%a(j)%x)
  deallocate(v%b(j)%x)
  deallocate(v%c(j)%x)
enddo
deallocate(v%a)
deallocate(v%b)
deallocate(v%c)

end program main
