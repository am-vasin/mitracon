*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                         ���� ��� � 䠩��.                            �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Where_F
PARAMETERS tit

PRIVATE ex, pth, pth_w, mss, sav_dir, sav_err, er_v

*                     < ��������� >
*�������������������������������������������������������������������Ŀ
*�                                                                   �
*�   ������ ���� � 䠩�� _________!_________!_________!_________!   �
*�                                                                   �
*�                 < OK Ctrl-W > < �⪠������ Esc >                  �
*�                                                                   �
*���������������������������������������������������������������������

pth = SPACE(40)
ex = 1

DO D_W WITH "Where_F", 8, 71, tit, 1

sav_dir = SET("DEFAULT")+CURDIR()

DO WHILE .T.
  ex = 1
  @ 3, 5 SAY "������ ���� � 䠩��" GET pth
  @ 5,19 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "
  READ CYCLE
  IF ex = 2
    pth_w = CHR(0)
    EXIT
  ENDIF
  pth_w = ALLTRIM(pth)
  IF LEN(pth_w) >= 2
    IF INLIST(UPPER(LEFT(pth_w,2)),"A:","B:")
      DO WHILE .NOT. FLOP(LEFT(pth_w,2))
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"�� ���뫨 ��⠢��� ��᪥��! ����� �㤥� ��� �襭��?"
        mss(3) = CHR(0)+"   [ ��⠢�塞 ��᪥�� ] [ ������ ���ன�⢮ ] [ �⪠����� ]"
        mss(4) = ""
        ex = Out_Mess(7, "mss")
        IF ex = 1
          LOOP
        ENDIF
        EXIT
      ENDDO
      IF ex = 2
        LOOP
      ENDIF
      IF ex = 3
        pth_w = CHR(0)
        EXIT
      ENDIF
    ENDIF
  ENDIF
  er_v = 0
  sav_err = ON("ERROR")
  ON ERROR er_v = 1
  SET DEFAULT TO (pth_w)
  SET DEFAULT TO (sav_dir)
  ON ERROR &sav_err
  IF er_v # 0
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�������� ���� ���� ����� � ���㤠. ������ ��� ��筨�."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    LOOP
  ENDIF
  EXIT
ENDDO

RELEASE WINDOW Where_F

RETURN pth_w