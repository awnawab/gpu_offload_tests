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

  type(container) :: a(m), b(m), c(m)

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
   c(j)%x(i) = a(j)%x(i) + b(j)%x(i)
enddo

!$acc end data
end subroutine kernel
end module kernel_mod

program main
use vars_mod, only: a,b,c
use kernel_mod, only: kernel
implicit none

integer :: i,j

do j=1,m
  c(j)%x = 0
  do i=1,n
    a(j)%x(i) = i + j
    b(j)%x(i) = 2*a(j)%x(i)
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
    if(c(j)%x(i) /= (a(j)%x(i) + b(j)%x(i)))then
       print *, i, j, c(j)%x(i), a(j)%x(i), b(j)%x(i)
       error stop
    endif
  enddo
enddo

end program main
