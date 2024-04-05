SetCapsLockState Off

CapsLock::
    ; Record the current timestamp
    start := A_TickCount

    ; Wait for CapsLock to be released
    While GetKeyState("CapsLock", "P")
        Sleep, 10

    ; If CapsLock was held for less than 150 milliseconds, send Esc
    If (A_TickCount - start < 150)
        Send {Esc}
return

#If GetKeyState("CapsLock", "P")
		h::Left
		j::Down
		k::Up
		l::Right
#If
