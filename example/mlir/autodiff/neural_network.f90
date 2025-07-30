program neural_network
    ! Simple neural network with automatic differentiation
    ! Single hidden layer network for XOR problem

    integer, parameter :: input_size = 2
    integer, parameter :: hidden_size = 3
    integer, parameter :: output_size = 1
    integer, parameter :: num_samples = 4
    integer, parameter :: max_epochs = 1000

    ! Network weights (simplified single layer for demonstration)
    real :: w1(input_size, hidden_size)
    real :: b1(hidden_size)
    real :: w2(hidden_size, output_size)
    real :: b2(output_size)

    ! Training data for XOR problem
    real :: inputs(input_size, num_samples)
    real :: targets(output_size, num_samples)

    ! Training parameters
    real, parameter :: learning_rate = 0.1
    real, parameter :: tolerance = 1.0e-4

    integer :: epoch, sample
    real :: total_loss, sample_loss
    real :: output(output_size)
    real :: hidden(hidden_size)

    print *, "Simple Neural Network with AD Example"
    print *, "Problem: XOR function"
    print *, "Architecture: 2 -> 3 -> 1"
    print *, ""

    ! Initialize training data
    call initialize_xor_data()

    ! Initialize weights randomly
    call initialize_weights()

    print *, "Initial weights:"
    call print_weights()
    print *, ""

    ! Training loop
    print *, "Training..."
    do epoch = 1, max_epochs
        total_loss = 0.0

        ! Process each training sample
        do sample = 1, num_samples
            ! Forward pass
            call forward_pass(inputs(:, sample), hidden, output)

            ! Compute loss (MSE)
            sample_loss = 0.5*(output(1) - targets(1, sample))**2
            total_loss = total_loss + sample_loss

            ! Backward pass (gradients computed by AD in real implementation)
            ! Here we show the structure for gradient computation
            call backward_pass(inputs(:, sample), hidden, output, targets(:, sample))
        end do

        ! Print progress
        if (mod(epoch, 100) == 1 .or. epoch <= 10) then
            print *, "Epoch", epoch, ": Loss =", total_loss/num_samples
        end if

        ! Check convergence
        if (total_loss/num_samples < tolerance) then
            print *, "Converged at epoch", epoch
            exit
        end if
    end do

    print *, ""
    print *, "Final weights:"
    call print_weights()

    print *, ""
    print *, "Testing trained network:"
    call test_network()

contains

    subroutine initialize_xor_data()
        ! XOR truth table
        ! Input:  [0,0] [0,1] [1,0] [1,1]
        ! Output:  [0]   [1]   [1]   [0]

        inputs(1, 1) = 0.0; inputs(2, 1) = 0.0; targets(1, 1) = 0.0
        inputs(1, 2) = 0.0; inputs(2, 2) = 1.0; targets(1, 2) = 1.0
        inputs(1, 3) = 1.0; inputs(2, 3) = 0.0; targets(1, 3) = 1.0
        inputs(1, 4) = 1.0; inputs(2, 4) = 1.0; targets(1, 4) = 0.0
    end subroutine initialize_xor_data

    subroutine initialize_weights()
        ! Simple weight initialization
        integer :: i, j

        ! Initialize weights with small random values
        do i = 1, input_size
            do j = 1, hidden_size
                w1(i, j) = (real(i + j)/10.0) - 0.5  ! Simple deterministic init
            end do
        end do

        do i = 1, hidden_size
            b1(i) = 0.1*real(i)
            do j = 1, output_size
                w2(i, j) = (real(i)/10.0) - 0.3
            end do
        end do

        b2(1) = 0.0
    end subroutine initialize_weights

    subroutine forward_pass(input, hidden_out, output_out)
        real, intent(in) :: input(input_size)
        real, intent(out) :: hidden_out(hidden_size)
        real, intent(out) :: output_out(output_size)
        integer :: i, j

        ! Hidden layer computation: h = tanh(W1*x + b1)
        do i = 1, hidden_size
            hidden_out(i) = b1(i)
            do j = 1, input_size
                hidden_out(i) = hidden_out(i) + w1(j, i)*input(j)
            end do
            hidden_out(i) = tanh(hidden_out(i))  ! Activation function
        end do

        ! Output layer computation: y = W2*h + b2
        do i = 1, output_size
            output_out(i) = b2(i)
            do j = 1, hidden_size
                output_out(i) = output_out(i) + w2(j, i)*hidden_out(j)
            end do
            ! No activation for output (linear)
        end do
    end subroutine forward_pass

    subroutine backward_pass(input, hidden_val, output_val, target)
        ! Simplified backward pass (in real AD implementation,
        ! gradients would be computed automatically)
        real, intent(in) :: input(input_size)
        real, intent(in) :: hidden_val(hidden_size)
        real, intent(in) :: output_val(output_size)
        real, intent(in) :: target(output_size)

        real :: output_error, hidden_error(hidden_size)
        real :: w1_grad(input_size, hidden_size)
        real :: w2_grad(hidden_size, output_size)
        real :: b1_grad(hidden_size), b2_grad(output_size)
        integer :: i, j

        ! Output layer error
        output_error = output_val(1) - target(1)

        ! Output layer gradients
        b2_grad(1) = output_error
        do i = 1, hidden_size
            w2_grad(i, 1) = output_error*hidden_val(i)
        end do

        ! Hidden layer errors (backpropagation)
        do i = 1, hidden_size
            hidden_error(i) = output_error*w2(i, 1)*(1.0 - hidden_val(i)**2)  ! tanh derivative
        end do

        ! Hidden layer gradients
        do i = 1, hidden_size
            b1_grad(i) = hidden_error(i)
            do j = 1, input_size
                w1_grad(j, i) = hidden_error(i)*input(j)
            end do
        end do

        ! Update weights (gradient descent)
        do i = 1, input_size
            do j = 1, hidden_size
                w1(i, j) = w1(i, j) - learning_rate*w1_grad(i, j)
            end do
        end do

        do i = 1, hidden_size
            b1(i) = b1(i) - learning_rate*b1_grad(i)
            do j = 1, output_size
                w2(i, j) = w2(i, j) - learning_rate*w2_grad(i, j)
            end do
        end do

        b2(1) = b2(1) - learning_rate*b2_grad(1)
    end subroutine backward_pass

    subroutine print_weights()
        integer :: i, j

        print *, "W1 (input to hidden):"
        do i = 1, input_size
            print *, (w1(i, j), j=1, hidden_size)
        end do

        print *, "b1 (hidden bias):"
        print *, (b1(i), i=1, hidden_size)

        print *, "W2 (hidden to output):"
        do i = 1, hidden_size
            print *, (w2(i, j), j=1, output_size)
        end do

        print *, "b2 (output bias):"
        print *, (b2(i), i=1, output_size)
    end subroutine print_weights

    subroutine test_network()
        integer :: i
        real :: test_input(input_size)
        real :: test_hidden(hidden_size)
        real :: test_output(output_size)

        do i = 1, num_samples
            test_input = inputs(:, i)
            call forward_pass(test_input, test_hidden, test_output)

            print *, "Input: [", test_input, "] Target:", targets(1, i), &
              " Output:", test_output(1), " Error:", abs(test_output(1) - targets(1, i))
        end do
    end subroutine test_network

end program neural_network
