module gauss_seidel

  use model_data

  implicit none
  save

  real(dp), private, parameter :: tol = 1e-5_dp

  contains

  ! Run the Gauss-Seidel algorithm until phi has converged
  subroutine generate_phi()
    implicit none
    real(dp) :: err
    integer :: i ! count iterations
    i = 1

    err = comptute_error()

    ! iterate until phi has converged
    do while (err > tol)
      call gs_iterate()
      err = comptute_error()
      i = i+1
    enddo
    print"(I0, A)", i, " iterations for phi to converge"
  end subroutine generate_phi

  ! Compute the electric field
  subroutine generate_e()
    implicit none
    integer :: i,j
    real(dp) :: dx_1, dy_1

    ! precompute some constants
    dx_1 = 0.5_dp/dx
    dy_1 = 0.5_dp/dx

    ! loop over the interior of the grid and compute the gradient of phi
    do j=1,ny
      do i=1,nx
        Ex(i,j) = (phi(i+1,j)-phi(i-1,j))*dx_1
        Ey(i,j) = (phi(i,j+1)-phi(i,j-1))*dy_1
      enddo
    enddo
  end subroutine generate_e

  ! Perform a single Gauss-Seidel iteration step
  subroutine gs_iterate()
    implicit none
    integer :: i,j
    real(dp) :: dx2, dy2, K1, K2

    ! precompute some constants
    dx2 = dx*dx
    dy2 = dy*dy
    K1 = dx2*dy2
    K2 = 1.0_dp/(2.0_dp*(dx2+dy2))

    ! loop over the interior of the grid and perform the iteration
    do j=1,ny
      do i=1,nx
        phi(i,j) = ((phi(i+1,j)+phi(i-1,j))*dy2 + (phi(i,j+1)+phi(i,j-1))*dx2 - rho(i,j)*K1)*K2
      enddo
    enddo
  end subroutine gs_iterate

  ! Compute the error as a ratio of etot/drms
  function comptute_error() result(err)
    implicit none
    real(dp) :: err, etot, drms, dx2phi, dy2phi
    integer :: i,j
    real(dp) :: dx2_1, dy2_1

    ! precompute some constants
    dx2_1 = 1.0_dp/(dx*dx)
    dy2_1 = 1.0_dp/(dy*dy)

    ! sum the total error and rms size of the gradient
    etot = 0.0_dp
    drms = 0.0_dp
    do j=1,ny
      do i=1,nx
        ! compute an interim value
        dx2phi = (phi(i-1,j)-2.0_dp*phi(i,j)+phi(i+1,j))*dx2_1
        dy2phi = (phi(i,j-1)-2.0_dp*phi(i,j)+phi(i,j+1))*dy2_1
        etot = etot + abs(dx2phi + dy2phi - rho(i,j))
        drms = drms + (dx2phi + dy2phi)*(dx2phi + dy2phi)
        ! drms = drms + (dx2phi*dx2phi + dy2phi*dy2phi)
      enddo
    enddo

    drms = sqrt(drms/(nx*ny))

    err = etot/drms
  end function comptute_error

end module gauss_seidel
