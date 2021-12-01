! ============================================================================ !
!   MODEL DATA                                                                 !
! ============================================================================ !
!   This module defines a set of global variables that are shared across       !
!   the entire project.                                                        !
module model_data

  use iso_fortran_env
  use domain_tools

  implicit none
  save

  ! define double precision
  integer, parameter :: dp = real64

  ! gridsize/spacing
  integer :: nx, ny
  real(dp) :: dx, dy

  ! axes
  real(dp), parameter, dimension(2) :: range = (/ -1.0_dp, 1.0_dp /)
  real(dp), dimension(:), allocatable :: x_axis
  real(dp), dimension(:), allocatable :: y_axis

  ! problem type
  character(len=6) :: problem

  ! grid variables
  real(dp), dimension(:,:), allocatable :: rho
  real(dp), dimension(:,:), allocatable :: phi
  real(dp), dimension(:,:), allocatable :: Ex
  real(dp), dimension(:,:), allocatable :: Ey

  contains

  ! allocates space for all of the data
  subroutine init_model_data()
    implicit none
    integer :: i,j

    ! pad rho and phi with ghost cells
    allocate(rho(0:nx+1,0:ny+1))
    allocate(phi(0:nx+1,0:ny+1))
    allocate(Ex(nx,ny))
    allocate(Ey(nx,ny))
    allocate(x_axis(0:nx+1))
    allocate(y_axis(0:nx+1))

    ! set the axes
    call create_axis(x_axis, nx, range, 1, dx)
    call create_axis(y_axis, ny, range, 1, dy)

    ! set the charge density depending on the problem
    rho = 0.0_dp ! default to null
    if (problem == "single") then
      do i=1,nx
        do j=1,ny
          ! single peak at (0,0)
          rho(i,j) = exp(-(x_axis(i)/0.1_dp)**2 - (y_axis(j)/0.1_dp)**2)
        enddo
      enddo
    else if (problem == "double") then
      do i=1,nx
        do j=1,ny
          ! double peak at (-0.25,-0.25) and (0.75,0.75)
          rho(i,j) = exp(-((x_axis(i)+0.25_dp)/0.1_dp)**2 - ((y_axis(j)+0.25_dp)/0.1_dp)**2) &
                   + exp(-((x_axis(i)-0.75_dp)/0.2_dp)**2 - ((y_axis(j)+0.75_dp)/0.2_dp)**2)
        enddo
      enddo
    endif
  end subroutine init_model_data

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
  subroutine init_args(err)
    implicit none
    integer, intent(out) :: err

    err = 0

    ! Parse all the command line args and read them
    call parse_args()

    ! attempts to read the problem type
    if (.not.get_arg("problem", problem)) then
      err = 1
      print"(A)", "ERROR: argument 'problem' not received"
    endif

    ! attempts to read nx
    if (.not.get_arg("nx", nx)) then
      err = 1
      print"(A)", "ERROR: argument 'nx' not received"
    endif

    ! attempts to read ny
    if (.not.get_arg("ny", ny)) then
      err = 1
      print"(A)", "ERROR: argument 'ny' not received"
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
