module cube_game
    implicit none

contains
    function convert_text_to_int(text) result (int)
        implicit none
        character(*),intent(in) :: text
        integer :: int
        integer :: err

        read(text,*,iostat=err) int
        if (err /= 0) then
            int = 0
        end if
    end function convert_text_to_int

    function min_color(sub_game_text, color) result (num_color)
        implicit none

        ! params
        character(*), intent(in) :: sub_game_text
        character(*), intent(in) :: color

        !local
        integer :: color_pos
        integer :: num_start_pos
        integer :: num_color

        num_color = 0
        color_pos = index(sub_game_text, color)
        if (color_pos > 0) then
            num_start_pos = scan(sub_game_text(:color_pos), ':,;', back = .true.)
            num_color = convert_text_to_int(trim(sub_game_text(num_start_pos+1:color_pos-1)))
            !print*, color
            !print*, num_color
        end if
    end function

    function min_colors_per_sub_game(sub_game_text) result (min_values)
        implicit none

        ! params
        character(*), intent(in) :: sub_game_text
        integer :: min_values(3)

        min_values(1) = min_color(sub_game_text, 'red')
        min_values(2) = min_color(sub_game_text, 'green')
        min_values(3) = min_color(sub_game_text, 'blue')
        !print*, min_values
    end function min_colors_per_sub_game

    function min_red_green_blue_for_game(game_text) result (min_values)
        implicit none

        ! params
        character(*), intent(in) :: game_text
        integer :: min_values(3)
        integer :: game_min_values(3)

        !local 
        integer :: start_pos
        integer :: end_pos
        character(5) :: game_id_text
        character(1000) :: sub_game_text

        if (game_text(:4) /= 'Game') then
            print*, 'Invalid Game Input'
            return
        end if 

        print*, 'Playing '//game_text
        start_pos = scan(game_text, ':') 

        min_values(:) = 0
        
        end_pos = scan(game_text(start_pos:), ';')
        do while (end_pos > 0)
            sub_game_text = game_text(start_pos:end_pos+start_pos)
            game_min_values = min_colors_per_sub_game(sub_game_text)
            if (game_min_values(1) > min_values(1)) then
                min_values(1) = game_min_values(1)
            end if
            if (game_min_values(2) > min_values(2)) then
                min_values(2) = game_min_values(2)
            end if
            if (game_min_values(3) > min_values(3)) then
                min_values(3) = game_min_values(3)
            end if
            start_pos = end_pos+start_pos
            end_pos = scan(game_text(start_pos:), ';')
        end do
        
        ! get the last subgame
        sub_game_text = game_text(start_pos:)
        game_min_values = min_colors_per_sub_game(sub_game_text)
        if (game_min_values(1) > min_values(1)) then
            min_values(1) = game_min_values(1)
        end if
        if (game_min_values(2) > min_values(2)) then
            min_values(2) = game_min_values(2)
        end if
        if (game_min_values(3) > min_values(3)) then
            min_values(3) = game_min_values(3)
        end if

    end function min_red_green_blue_for_game

end module cube_game

program aoc_2023_02
    use cube_game
    implicit none

    character(2048) :: game_text
    integer :: io, err
    integer :: x(3)
    integer :: power
    integer :: power_sum

    x = 0
    power_sum = 0

    io = 55 ! I think this can be anything? 
    open(newunit=io, file='games.txt', status='old', action='read')
    do
        read(io, '(A)', iostat=err) game_text
        if (err /= 0) exit
        x = min_red_green_blue_for_game(trim(game_text))
        print*, 'Min Red='
        print*, x(1)
        print*, 'Min Green='
        print*, x(2)
        print*, 'Min Blue='
        print*, x(3)
        power = x(1) * x(2) * x(3)
        print*, 'Power='
        print*, power
        power_sum = power_sum + power
    end do
    close(io)

    print*, ''
    print*, 'Final Sum = '
    print*, power_sum
end program aoc_2023_02