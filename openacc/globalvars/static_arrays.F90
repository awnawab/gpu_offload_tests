! (C) Copyright 2023- ECMWF.
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

#define n 32
#define m 128

module vars_mod
  integer :: a(n,m)
  integer :: b(n,m)
  integer :: c(n,m)

!$acc declare create(a,b,c)
end module vars_mod

module kernel_mod
contains
subroutine kernel(j)
use vars_mod, only: a,b,c
implicit none

integer, intent(in) :: j
integer :: i
!$acc routine vector
!$acc data present(a,b,c)

!$acc loop vector
do i=1,n
   c(i,j) = a(i,j) + b(i,j)
enddo

!$acc end data
end subroutine kernel
end module kernel_mod

program main
use vars_mod
use kernel_mod, only: kernel
implicit none

integer :: i,j

c = 0
do j=1,m
  do i=1,n
    a(i,j) = i + j
    b(i,j) = 2*a(i,j)
  enddo
enddo

!$acc update device(a,b,c)

!$acc parallel loop gang
do j=1,m 
   call kernel(j)
enddo

!$acc update self(c)

do j=1,m
  do i=1,n
    if(c(i,j) /= (a(i,j) + b(i,j)))then
       print *, i, j, c(i,j), a(i,j), b(i,j)
       error stop
    endif
  enddo
enddo

end program main
