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



! ============================================================================ !
!   READ INPUT                                                                 !
! ============================================================================ !
!   This module provides a set of functions to read and validate user input.   !                                                     !
module read_input

  use command_line
  use model_data

  implicit none
  save

  contains

  ! Attempts to read the arguments from the command line, and sets defaults if
  ! an argument is missing.
  subroutine init_args()
    implicit none
    logical :: has_nx, has_ny

    ! Parse all the command line args and read them
    call parse_args()

    ! attempts to read the problem type, defaulting to null
    if (.not.get_arg("problem", problem)) then
      problem = "null"
      print"(A, A)", "WARNING: argument 'problem' not received, defaulting to ", problem
    endif

    ! attempts to read nx and ny
    has_nx = get_arg("nx", nx)
    has_ny = get_arg("ny", ny)

    ! if one has been supplied but not the other, make them equal
    if (has_nx .and. (.not.has_ny)) then
      ny = nx
      print"(A, I0)", "WARNING: argument 'ny' not received, matching nx: ", nx
    else if (has_ny .and. (.not.has_nx)) then
      nx = ny
      print"(A, I0)", "WARNING: argument 'nx' not received, matching ny: ", ny
    endif

    ! if neither is supplied then default to 50
    if ((.not.has_nx) .and. (.not.has_ny)) then
      nx = 50
      ny = 50
      print"(A, I0)", "WARNING: argument 'nx' not received, defaulting to ", nx
      print"(A, I0)", "WARNING: argument 'ny' not received, defaulting to ", ny
    endif
  end subroutine init_args

  ! Ensures that the problem parameters contain valid values. Sets err to 1
  ! if an argument is invalid, otherwise sets it to 0.
  subroutine validate_args(err)
    implicit none
    integer, intent(out) :: err ! error flag
    err = 0

    ! gridsizes must be positive
    if (nx <= 0) then
      print"(A)", "ERROR: 'nx' must be greater than 0"
      err = 1
    endif
    if (ny <= 0) then
      print"(A)", "ERROR: 'ny' must be greater than 0"
      err = 1
    endif

    ! Convert the bnd key to lowercase and check it is permitted
    IF (problem /= "null" .and. problem /= "single" .and. problem /= "double") THEN
      print"(A)", "ERROR: 'problem' must be chosen from: 'null', 'single', 'double'"
      err = 1
    ENDIF
  end subroutine validate_args

end module read_input
