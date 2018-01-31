program end_date

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! N. Jourdain
!
! input : year, month, day, duration
!
! output : yearf, monthf, dayf (final if run length = duration)
!          durcorr (corrected duration to finish :
!                       - not after the end of current year
!                       - at the end of a finite month      )
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

IMPLICIT NONE

INTEGER :: an0, mois0, jour0, duration ! duration in nb days

INTEGER :: an, mois, jour, durleft, durcorr

INTEGER, DIMENSION(12) :: ndays_month

LOGICAL :: tocken

ndays_month = (/31,28,31,30,31,30,31,31,30,31,30,31/)

open(11,file='start_date_duration')

read(11,*) an0, mois0, jour0, duration

tocken = .true.

durleft = duration
durcorr = duration
an      = an0
mois    = mois0
jour    = jour0

do while ( durleft .gt. 0 )

  if ( (jour+durleft) .gt. ndays_month(mois) ) then

    if ( mois .eq. 12 ) then

      durleft = durleft - ( ndays_month(mois) - jour + 1 )
      if ( tocken ) then
        durcorr = duration - durleft
        tocken = .false.
      endif
      an      = an + 1
      mois    = 1
      jour    = 1

    else

      durleft = durleft - ( ndays_month(mois) - jour + 1 )
      mois    = mois + 1
      jour    = 1

    endif

  else

    jour    = jour + durleft
    durleft = 0

  endif

enddo

!====================
if ( jour .ne. 1 .and. an .eq. an0 ) then
  durcorr = durcorr - jour + 1
  !jour = 1
endif
!====================

write(*,898) an, mois, jour, durcorr
898 FORMAT(i4.4,' ',i2.2,' ',i2.2,' ',i3)

close(11)

end program end_date
