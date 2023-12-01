program aoc_2023_01
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

    do while (num_blank < 1)
      read(*,fmt='(A)') text
      line_value_text = ''
      first_num = .false.

      do i=1, len_trim(text)
        read(text(i:i), *, iostat=err) x
        if (err == 0 .and. .not. first_num) then
            line_value_text(1:1) = text(i:i)
            first_num = .true.
        elseif (err == 0 .and. first_num) then
            line_value_text(2:2) = text(i:i)
        endif
      enddo
      if (len_trim(line_value_text) == 1) then 
          line_value_text(2:2) = line_value_text(1:1)
      endif

      if (len_trim(line_value_text) > 1) then
          read(line_value_text, *) line_value
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
