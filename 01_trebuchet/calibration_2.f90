module codez
    implicit none

contains
    function get_num_from_text(text, start, search_text, val) result (val2)
        implicit none
        character(50) :: text
        character(*) :: search_text
        integer :: i
        integer :: start
        integer :: val
        integer :: val2
        logical :: match 

        val2 = 0
        i = 1
        ! default to assuming a match
        match = .true.

        do i=1, len_trim(search_text)
            if (text(start+i-1:start+i-1) /= search_text(i:i)) then
                match = .false.
            endif
        enddo

        if (match) then
            val2 = val
        endif
    end function get_num_from_text
end module codez

program aoc_2023_01
    use codez
    implicit none

    character(50) :: text
    character(2) :: line_value_text
    integer :: num_blank
    integer :: line_value
    integer :: calibration_value
    logical :: first_num
    integer :: i
    integer :: x
    integer :: err

    num_blank = 0
    calibration_value = 0

    ! read stdin until there is an empty line
    do while (num_blank < 1)
      read(*,fmt='(A)') text
      line_value_text = ''
      first_num = .false.

      do i=1, len_trim(text)
        read(text(i:i), *, iostat=err) x
        if (err /= 0) then
            ! error converting to int check for string
            x = get_num_from_text(text, i, 'one', 1)
            if (x == 0) then
                x = get_num_from_text(text, i, 'two', 2)
            end if
            if (x == 0) then
                x = get_num_from_text(text, i, 'three', 3)
            end if
            if (x == 0) then
                x = get_num_from_text(text, i, 'four', 4)
            end if
            if (x == 0) then
                x = get_num_from_text(text, i, 'five', 5)
            end if
            if (x == 0) then
                x = get_num_from_text(text, i, 'six', 6)
            end if
            if (x == 0) then
                x = get_num_from_text(text, i, 'seven', 7)
            end if
            if (x == 0) then
                x = get_num_from_text(text, i, 'eight', 8)
            end if
            if (x == 0) then
                x = get_num_from_text(text, i, 'nine', 9)
            end if
        endif

        if (x /= 0 .and. .not. first_num) then
            write(line_value_text(1:1), '(i1)' ) x
            first_num = .true.
        elseif (x /= 0 .and. first_num) then
            write(line_value_text(2:2), '(i1)') x
        endif
      enddo
      if (len_trim(line_value_text) == 1) then 
          line_value_text(2:2) = line_value_text(1:1)
      endif

      if (len_trim(line_value_text) > 1) then
          read(line_value_text, '(i2)') line_value
          calibration_value = calibration_value + line_value
      endif

      if (len_trim(text) == 0) then
         num_blank = num_blank + 1
      else
         num_blank = 0
      endif
    enddo

    !print *, 'Calibration Value = '//calibration_value
    print *, 'Calibration Value = '
    print *, calibration_value

end program aoc_2023_01


