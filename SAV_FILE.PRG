*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                蹍������ 甌縲��キ�� 甎�瓷� ������.                    �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Sav_File
PARAMETERS f_list
*
*    f_list - ��瘁│ 甌 甎�瓷�� ������ か� �ムイ�腮:
*                  f_list(i,1) - ━� �痳�き��� �����;
*                  f_list(i,2) - ━� �����-爛с�赳���;
*                  f_list(i,1) - prompt.
*
PRIVATE l_max, l_arr, i, wd, pth, sz_req, def_sav, sz_exist, mss, j, dmmy
l_arr = ALEN(f_list,1)
IF l_arr = 0    && �モ 甎�瓷�
  RETURN
ENDIF

DIMENSION f_list(l_arr, 3)

l_max = 0     && Max か┃� �牀����
sz_req = 0    && ｡薑� ��Кム 甌縲��錺�諷 ������
FOR i = 1 TO l_arr
  sz_req = sz_req+Fil_Size(f_list(i, 1))
  IF l_max < LEN(f_list(i, 3))
    l_max = LEN(f_list(i, 3))
  ENDIF
ENDFOR

def_sav = SET("DEFAULT")+CURDIR()

DO WHILE .T.
  pth = Where_F("�磴� ��茱� ����襯?")
  IF pth = CHR(0)
    RETURN
  ENDIF
  i = 0
  DO WHILE .T.
    SET DEFAULT TO (pth)
    sz_exist = DISKSPACE()
    SET DEFAULT TO (def_sav)
    IF sz_exist < sz_req      && �モ �メ�� �� え瓷�
      DIMENSION mss(4)
      mss(1) = ""
      mss(2) = CHR(0)+"�� え瓷� �モ �メ��: ━ゥ� "+ALLTRIM(STR(sz_exist,15))+ ;
                       ", ��ぎ "+ALLTRIM(STR(sz_req,15))+"."
      mss(3) = CHR(0)+"   �皰 ＜ぅ� ぅ��碎? [ ��腮痰━ え瓷 ] [ ��キ━ 竅矗��痰〓 ] [ �皖�Ε�瘴 ]"
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
          mss(2) = CHR(0)+"  ���� "+wd+". ����錺�? [ �� ] [ �モ ]"
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
DO D_W WITH "Sav_File", 6, wd, "����瘡", 1

wd = SET("CURSOR") = "ON"
SET CURSOR OFF
FOR i = 1 TO l_arr
  @ 3, 2 SAY PADC(f_list(i, 3), WCOLS()-4)
  IF ADIR(dmmy,pth+ALLTRIM(f_list(i, 2))) > 0
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  ���� � ━キガ "+ALLTRIM(f_list(i, 2))+" 礒� メ碎. ����錺�? [ �� ] [ �モ ]"
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

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                蹍������ ��爛ぅ�キ�� ��Кム� �����.                   �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Fil_Size
PARAMETERS f_name

PRIVATE a_w

IF ADIR(a_w,f_name) = 0
  RETURN 0
ENDIF
RETURN a_w(1, 2)
