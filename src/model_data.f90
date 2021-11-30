! ============================================================================ !
!   MODEL DATA                                                                 !
! ============================================================================ !
!   This module defines a set of global variables that are shared across       !
!   the entire project.                                                        !
module model_data

  use iso_fortran_env

  implicit none
  save

  ! define double precision
  integer, parameter :: dp = real64

  ! gridsize
  integer :: nx, ny

  ! problem type
  character(len=6) :: problem

  ! grid variables
  real(dp), dimension(:,:), allocatable :: rho
  real(dp), dimension(:,:), allocatable :: phi
  real(dp), dimension(:,:), allocatable :: Ex
  real(dp), dimension(:,:), allocatable :: Ey

  contains

end module model_data
