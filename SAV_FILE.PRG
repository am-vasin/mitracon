*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                �ணࠬ�� ��࠭���� ᯨ᪠ 䠩���.                    �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Sav_File
PARAMETERS f_list
*
*    f_list - ���ᨢ � ᯨ᪮� 䠩��� ��� ��।��:
*                  f_list(i,1) - ��� ��室���� 䠩��;
*                  f_list(i,2) - ��� 䠩��-१����;
*                  f_list(i,1) - prompt.
*
PRIVATE l_max, l_arr, i, wd, pth, sz_req, def_sav, sz_exist, mss, j, dmmy
l_arr = ALEN(f_list,1)
IF l_arr = 0    && ��� ᯨ᪠
  RETURN
ENDIF

DIMENSION f_list(l_arr, 3)

l_max = 0     && Max ����� �஬��
sz_req = 0    && ��騩 ࠧ��� ��࠭塞�� 䠩���
FOR i = 1 TO l_arr
  sz_req = sz_req+Fil_Size(f_list(i, 1))
  IF l_max < LEN(f_list(i, 3))
    l_max = LEN(f_list(i, 3))
  ENDIF
ENDFOR

def_sav = SET("DEFAULT")+CURDIR()

DO WHILE .T.
  pth = Where_F("�㤠 ��襬 �����?")
  IF pth = CHR(0)
    RETURN
  ENDIF
  i = 0
  DO WHILE .T.
    SET DEFAULT TO (pth)
    sz_exist = DISKSPACE()
    SET DEFAULT TO (def_sav)
    IF sz_exist < sz_req      && ��� ���� �� ��᪥
      DIMENSION mss(4)
      mss(1) = ""
      mss(2) = CHR(0)+"�� ��᪥ ��� ����: ����� "+ALLTRIM(STR(sz_exist,15))+ ;
                       ", ���� "+ALLTRIM(STR(sz_req,15))+"."
      mss(3) = CHR(0)+"   �� �㤥� ������? [ ����⨬ ��� ] [ ������ ���ன�⢮ ] [ �⪠����� ]"
      mss(4) = ""
      j = Out_Mess(7,"mss")
      DO CASE
      CASE j = 1
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = ""
        mss(3) = ""
        wd = Fdd_Dir(pth, sz_req)
        DO WHILE .NOT. EMPTY(wd)
          mss(2) = CHR(0)+"  ���� "+wd+". ����塞? [ �� ] [ ��� ]"
          IF Out_Mess(7,"mss") = 1
            DELETE FILE (wd)
          ENDIF
          wd = Fdd_Dir(pth, sz_req)
        ENDDO
        LOOP
      CASE j = 2
        i = 1
      CASE j = 1
        pth = CHR(0)
      ENDCASE
    ENDIF
    EXIT
  ENDDO
  IF i = 0
    EXIT
  ENDIF
ENDDO

IF pth = CHR(0)
  RETURN
ENDIF

wd = MAX(l_max+8, 10)
DO D_W WITH "Sav_File", 6, wd, "������", 1

wd = SET("CURSOR") = "ON"
SET CURSOR OFF
FOR i = 1 TO l_arr
  @ 3, 2 SAY PADC(f_list(i, 3), WCOLS()-4)
  IF ADIR(dmmy,pth+ALLTRIM(f_list(i, 2))) > 0
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  ���� � ������ "+ALLTRIM(f_list(i, 2))+" 㦥 ����. ����塞? [ �� ] [ ��� ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 2
      LOOP
    ENDIF
    DELETE FILE (pth+ALLTRIM(f_list(i, 2)))
  ENDIF
  COPY FILE (f_list(i,1)) TO (pth+ALLTRIM(f_list(i, 2)))
ENDFOR

IF wd
  SET CURSOR ON
ENDIF

RELEASE WINDOW "Sav_File"
RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                �ணࠬ�� ��।������ ࠧ��� 䠩��.                   �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Fil_Size
PARAMETERS f_name

PRIVATE a_w

IF ADIR(a_w,f_name) = 0
  RETURN 0
ENDIF
RETURN a_w(1, 2)