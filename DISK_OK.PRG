*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Disk_OK      ��о�｀砒┴ ��むォ ��瓱�           13.02.97 15:05:58 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�    蹍▲爲� 竅矗��痰�� �� �爛がモ: � �� え瓷モ� �� � 竅����←キ� ��?   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Disk_OK
PARAMETER pth

PRIVATE mss, drv

IF LEN(pth) < 2
  RETURN .T.
ENDIF

IF INLIST(UPPER(LEFT(pth,2)), "A:", "B:")   && Floppy disk!
  DO WHILE .T.
    drv = UPPER(LEFT(pth,1))
    CALL IsDisk WITH drv
    IF drv # UPPER(LEFT(pth,1))
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"  � え瓷モ� ＜ぅ� 竅����←│�碎? [ �� ] [ �モ ]"
      mss(3) = ""
      IF Out_Mess(7, "mss") = 1
        LOOP
      ENDIF
      RETURN .F.
    ENDIF
    EXIT
  ENDDO
ENDIF

RETURN .T.
