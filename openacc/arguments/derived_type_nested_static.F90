! (C) Copyright 2023- ECMWF.
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

#define n 32
#define m 128

module vars_mod
  type :: container
     integer :: x(n)
  end type container

  type :: bucket
     type(container) :: a(m), b(m), c(m)
  end type bucket
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
use vars_mod, only: bucket
use kernel_mod, only: kernel
implicit none
type(bucket) :: v

integer :: i,j

do j=1,m
  v%c(j)%x = 0
  do i=1,n
    v%a(j)%x(i) = i + j
    v%b(j)%x(i) = 2*v%a(j)%x(i)
  enddo
enddo

!$acc enter data copyin(v)

!$acc parallel loop gang
do j=1,m 
   call kernel(v%a(j), v%b(j), v%c(j))
enddo

!$acc exit data copyout(v)

do j=1,m
  do i=1,n
    if(v%c(j)%x(i) /= (v%a(j)%x(i) + v%b(j)%x(i)))then
       print *, i, j, v%c(j)%x(i), v%a(j)%x(i), v%b(j)%x(i)
       error stop
    endif
  enddo
enddo

end program main
