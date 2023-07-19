! (C) Copyright 2023- ECMWF.
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

#define m 128

module vars_mod
  type :: container
     integer, allocatable :: x(:)
  end type container
end module vars_mod

module kernel_mod
contains
subroutine kernel(n,a,b,c)
use vars_mod, only: container
implicit none

integer, intent(in) :: n
type(container), intent(in) :: a,b
type(container), intent(inout) :: c
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
integer :: i,j,n
type(container) :: a(m), b(m), c(m)

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

!$acc enter data create(a,b,c)
do j=1,m
!$acc enter data copyin(a(j)%x, b(j)%x)
!$acc enter data create(c(j)%x)
enddo

!$acc parallel loop gang
do j=1,m
  call kernel(n,a(j),b(j),c(j))
enddo

do j=1,m
!$acc exit data delete(a(j)%x, b(j)%x)
!$acc exit data copyout(c(j)%x)
enddo
!$acc exit data delete(a,b,c)

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
