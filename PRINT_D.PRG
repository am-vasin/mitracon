*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                   �
*�               蹍������ �ョ�皋/�牀甃�矗� ぎ�祠キ��.               �
*�                                                                   �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Print_D
PARAMETERS ext_menu, f_nam, d_nm, pg_l, flt, fnt, ncp, f_end, dbf_n, p_d, ;
           c_code, winType, winFirm, winDoc, WinDate
PRIVATE net_mode
net_mode = .T.

DO Entry_P WITH ext_menu, ""

PRIVATE ln, wd, l1, c1, dbf_w, pic, c00, p_len, p_list, s_sav

dbf_w = IIF(TYPE("dbf_n") = "C", ALLTRIM(dbf_n), "")
IF .NOT. EMPTY(dbf_w)
  dbf_w = IIF("." $ dbf_w, dbf_w, dbf_w+".DBF")
ENDIF
w_sav = WOUTPUT()
ln = 14
wd = 66
p_len = 0
DIMENSION p_list(1)
p_list(1) = SPACE(32)
s_sav = SELECT()
SELECT 0
USE (base_path+"PRN_DIRS")
SCAN
  p_len = p_len+1
  DIMENSION p_list(p_len)
  p_list(p_len) = NAME
ENDSCAN
USE
SELECT (s_sav)
p_len = MAX(p_len,1)

PRIVATE p_s, p_e, l_f, n_c, trm, frg, t_f, f_sw, f_num, p_d0
p_s = 0
p_e = 0
frg = .T.
trm = 1
l_f = flt
p_d0 = IIF(BETWEEN(p_d, 1, p_len), p_d, 1)
n_c = ncp
IF f_end < 0
  f_sw = 1
  f_num = 0
ELSE
  f_sw = 2
  f_num = f_end
ENDIF

IF prn_mode
  DO WHILE .T.
    DO P_Screen WITH ALLTRIM(d_nm), .NOT. EMPTY(dbf_w)
    *n_c = ncp
    DO CASE
    CASE trm = 1   && �ョ�碎
      IF net_mode
        DO Prnt_Txt_N WITH f_nam, pg_l, l_f, n_c, p_s, p_e, fnt, IIF(f_sw=1, -1, f_num), p_d0
      ELSE
        DO Prnt_Txt WITH f_nam, pg_l, l_f, n_c, p_s, p_e, fnt, IIF(f_sw=1, -1, f_num), p_d0
      ENDIF
      EXIT
    CASE trm = 2   && 蹍甃�矗
      DO View_Txt WITH f_nam, d_nm, IIF(pg_l=0, 66, pg_l), l_f, fnt
    CASE trm = 5   && WordPad
      DO P_Wind WITH f_nam
      EXIT
    CASE trm = 3   && ����瘡 � ���� TXT
      DO Txt_To_F WITH f_nam
    CASE trm = 4       && ����瘡 � DBF-����
      DO DBF_To_F WITH dbf_w
    CASE trm = 6       && ����
      DO S_Fax WITH ALLTRIM(d_nm), c_code
    CASE trm = 7       && E_Mail
      DO S_Mail WITH ALLTRIM(d_nm), c_code, f_nam, dbf_n
    CASE trm = 8       && �ョ�碎 Win_Sale
      DO To_WinSale
    OTHERWISE
      EXIT
    ENDCASE

  ENDDO
ELSE
  IF net_mode
    DO Prnt_Txt_N WITH f_nam, pg_l, l_f, n_c, p_s, p_e, fnt, IIF(f_sw=1, -1, f_num), p_d0
  ELSE
    DO Prnt_Txt WITH f_nam, pg_l, l_f, n_c, p_s, p_e, fnt, IIF(f_sw=1, -1, f_num), p_d0
  ENDIF
ENDIF

RELEASE WINDOW Prnt_D
IF EMPTY(w_sav)
  ACTIVATE SCREEN
ELSE
  ACTIVATE WINDOW (w_sav)
ENDIF

DO Exit_P WITH ext_menu, ""

SELECT (s_sav)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                   �
*�                     蹍������ �ョ�皋 ぎ�祠キ��.                   �
*�                                                                   �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Prnt_Txt
PARAMETERS f_nam, pg_l, l_f, n_c, p_s, p_e, fnt, l_s, p_d
*
*  f_nam - ━� 皀�痰�〓�� �����;
*  pg_l  - か┃� 痰����肓;
*  l_f   - ���� 甄ア�;
*  n_c   - 腮甄� ���┤;
*  p_s   - �����讚�� 痰������;
*  p_e   - ���ョ��� 痰������;
*  fnt   - 葯�籵;
*  l_s   - ��▲琥キ┘ �ョ�皋.
*

PRIVATE buff, pag_no, i, j, n, c_cnt, n_f, sw, p_u, s_sav, p_end

PRIVATE l1, c1, l2, c2, w_sav, ff, s, p_type_n, pt_ok, p_end
s_sav = SELECT()
p_type_n=0
IF .NOT. File_O(prmo_path+"OWN_DIRS.DBF")
  p_u = "PRN:"
ELSE
  SELECT 0
  USE (prmo_path+"OWN_DIRS.DBF")
  * �痰� �� ���� PRNTYPE (腮甄�〓�) � ��゛�罐 OWN_DIRS?
  pt_ok = FSIZE("PRNTYPE") # 0
  IF p_d > 0 .AND. p_d <= RECCOUNT()
    GO p_d
    p_u = ALLTRIM(PORT_NAME)
    IF pt_ok
      p_type_n=OWN_DIRS.PRNTYPE && �┓ �爬�皀�� = ���ム ����瓱 � PRNSETUP.DBF
    ENDIF
  ELSE
    p_u = "PRN:"
  ENDIF
  IF EMPTY(p_u)
    p_u = "PRN:"
  ENDIF
  USE
ENDIF
SELECT (s_sav)

ff = CHR(12)
l2 = 8
c2 = 36
w_sav = WOUTPUT()
DO D_W WITH "Pr", l2, c2, "�ョ�碎 ぎ�祠キ��", 1

@ 3, 4 SAY "����� N"
@ 3,18 SAY "�矗����� N"
c1 = 12
c2 = 29

SET PRINTER TO &p_u
*SET PRINTER TO TMP_PRN.TXT &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
*IF File_O(base_path+"INI.PRN")
*  n = FOPEN(base_path+"INI.PRN")
*  buff = FREAD(n,2000)
*  = FCLOSE(n)
*ELSE
*  buff = CHR(27)+"@"+CHR(27)+"C"+CHR(pg_l)+CHR(27)+"l"+CHR(l_f)+ ;
*  IIF(EMPTY(fnt), "", CHR(15))
*ENDIF
*buff = P_Ctrl(pg_l)+IIF(EMPTY(fnt), CHR(18), CHR(15))
buff = ''
p_end = .F.
IF !EMPTY(p_type_n)
  buff = buff+P_Ctrl(pg_l, fnt, p_type_n, p_end)
ENDIF


n = LEN(buff)
FOR i = 1 TO n
  ??? SUBSTR(buff,i,1)
ENDFOR

FOR c_cnt = 1 TO n_c
  @ 3, c1 SAY STR(c_cnt,2)
  n_f = FOPEN(f_nam)
  pag_no = 1
  @ 3, c2 SAY STR(pag_no,3)
  j = p_s = 0 .OR. BETWEEN(pag_no, p_s, p_e)
  l1 = IIF(j, "�ョ���ガ...", "蹍�竅��ガ...")
  sw = .F.
  @ 5, 2 SAY PADC(l1,WCOLS()-4)
  DO WHILE .T.
    IF FEOF(n_f)
      EXIT
    ENDIF
    buff = SPACE(l_f)+FGET(n_f, 500)
    n = LEN(buff)
    FOR i = 1 TO n
      s = SUBSTR(buff,i,1)
      IF j # sw
        ??? s
      ENDIF
      sw = .F.
      IF s = ff
        pag_no = pag_no+1
        @ 3, c2 SAY STR(pag_no,3)
        sw = j
        j = p_s = 0 .OR. BETWEEN(pag_no, p_s, p_e)
        sw = j # sw
        l1 = IIF(j, "�ョ���ガ...", "蹍�竅��ガ...")
        @ 5, 2 SAY PADC(l1,WCOLS()-4)
      ENDIF
    ENDFOR
  ENDDO
  = FCLOSE(n_f)
ENDFOR

IF l_s < 0
  ??? CHR(12)
ELSE
  FOR i = 1 TO l_s
    ??? CHR(10)
  ENDFOR
ENDIF
*buff = buff+P_Ctrl()

p_end = .T.
IF !EMPTY(p_type_n)
  buff = buff+P_Ctrl(pg_l, fnt, p_type_n, p_end)
ENDIF

n = LEN(buff)
FOR i = 1 TO n
  ??? SUBSTR(buff,i,1)
ENDFOR

RELEASE WINDOW Pr

IF EMPTY(w_sav)
  ACTIVATE SCREEN
ELSE
  ACTIVATE WINDOW (w_sav)
ENDIF

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                   �
*�              蹍������ ����瓱 ぎ�祠キ�� � ����.                   �
*�                                                                   �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Txt_To_F
PARAMETERS f_nam

PRIVATE new_f, f_sz, a_w
= ADIR(a_w, f_nam)
f_sz = a_w(1,2)
new_f = New_File("", "TXT", "�磴� ��茱� ぎ�祠キ�?", f_sz)

IF .NOT. EMPTY(new_f)
  DO Wt_Mess WITH "����瘡..."
  new_f = ALLTRIM(new_f)
  COPY FILE (f_nam) TO (new_f)
  DO Wt_Mess
ENDIF

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                   �
*�                   蹍������ ����瓱 DBF-�����.                     �
*�                                                                   �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE DBF_To_F
PARAMETERS f_nam
PRIVATE new_f, f_sz, a_w
= ADIR(a_w, f_nam)
f_sz = a_w(1,2)
new_f = New_File("", "DBF", "�磴� ��茱� DBF-����?", f_sz)

IF .NOT. EMPTY(new_f)
  DO Wt_Mess WITH "����瘡..."
  new_f = ALLTRIM(new_f)
  COPY FILE (f_nam) TO (new_f)
  DO Wt_Mess
ENDIF
*DELETE FILE(f_nam)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                   �
*�                     蹍������ �ョ�皋 ぎ�祠キ��.                   �
*�                                                                   �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Prnt_Txt_N
PARAMETERS f_nam, pg_l, l_f, n_c, p_s, p_e, fnt, l_s, p_d
*
*  f_nam - ━� 皀�痰�〓�� �����;
*  pg_l  - か┃� 痰����肓;
*  l_f   - ���� 甄ア�;
*  n_c   - 腮甄� ���┤;
*  p_s   - �����讚�� 痰������;
*  p_e   - ���ョ��� 痰������;
*  fnt   - 葯�籵;
*  l_s   - ��▲琥キ┘ �ョ�皋.
*

PRIVATE buff, pag_no, i, j, n, c_cnt, n_f, sw, f_out, p_u, p_end, lj

PRIVATE l1, c1, l2, c2, w_sav, ff, s, s_sav, p_type_n, pt_ok
s_sav = SELECT()
lj = .F.

p_type_n=0
IF .NOT. File_O(prmo_path+"OWN_DIRS.DBF")
  p_u = "PRN:"
ELSE
  SELECT 0
  USE (prmo_path+"OWN_DIRS.DBF")

  * �痰� �� ���� PRNTYPE (腮甄�〓�) � ��゛�罐 OWN_DIRS?
  pt_ok = FSIZE("PRNTYPE") # 0

  IF p_d > 0 .AND. p_d <= RECCOUNT()
    GO p_d
    p_u = ALLTRIM(PORT_NAME)
    IF pt_ok
      p_type_n=OWN_DIRS.PRNTYPE
    ENDIF
  ELSE
    p_u = "PRN:"
  ENDIF
  IF EMPTY(p_u)
    p_u = "PRN:"
  ENDIF
  USE
ENDIF
SELECT (s_sav)

ff = CHR(12)
l2 = 8
c2 = 36
w_sav = WOUTPUT()
DO D_W WITH "Pr", l2, c2, "�ョ�碎 ぎ�祠キ��", 1

@ 3, 4 SAY "����� N"
@ 3,18 SAY "�矗����� N"
c1 = 12
c2 = 29

SET PRINTER TO &p_u
*SET PRINTER TO TMP_PRN.TXT &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
DELETE FILE (tmpo_path+"PRN_TMP.TXT")
f_out = FCREATE(tmpo_path+"PRN_TMP.TXT")

*IF File_O(base_path+"INI.PRN")
*  n = FOPEN(base_path+"INI.PRN")
*  buff = FREAD(n,2000)
*  = FCLOSE(n)
*ELSE
*  buff = CHR(27)+"@"+CHR(27)+"C"+CHR(pg_l)+CHR(27)+"l"+CHR(l_f)+ ;
*  IIF(EMPTY(fnt), "", CHR(15))
*ENDIF
*buff = CHR(27)+"@"+CHR(27)+"C"+CHR(pg_l)+IIF(EMPTY(fnt), "", CHR(15))

buff = ''
p_end = .F.
IF !EMPTY(p_type_n)
  buff = buff+P_Ctrl(pg_l, fnt, p_type_n, p_end)

  IF File_O(base_path+"PRNSETUP.DBF")
    SELECT 0
    USE (base_path+"PRNSETUP.DBF") SHARED AGAIN IN 0
  ENDIF

  GO p_type_n IN PRNSETUP
  IF TYPE("PRNSETUP.IS_LJ") = "L"
    lj = PRNSETUP.IS_LJ
  ENDIF
  USE IN PRNSETUP

ENDIF

*@ 3, c1 SAY STR(c_cnt,2)
n_f = FOPEN(f_nam)
pag_no = 1
@ 3, c2 SAY STR(pag_no,3)
j = p_s = 0 .OR. BETWEEN(pag_no, p_s, p_e)
l1 = IIF(j, "�ョ���ガ...", "蹍�竅��ガ...")
sw = j
@ 5, 2 SAY PADC(l1,WCOLS()-4)
DO WHILE .NOT. FEOF(n_f)
  buff = buff+SPACE(l_f)+FGET(n_f, 500)
  IF RIGHT(buff,1) = ff
    pag_no = pag_no+1
    @ 3, c2 SAY STR(pag_no,3)
    sw = j
    j = p_s = 0 .OR. BETWEEN(pag_no, p_s, p_e)
    l1 = IIF(j, "�ョ���ガ...", "蹍�竅��ガ...")
    @ 5, 2 SAY PADC(l1,WCOLS()-4)
  ELSE
*    p_end=.F.
*    IF !EMPTY(p_type_n)
*      buff = buff+P_Ctrl(pg_l, fnt, p_type_n, p_end)
*    ENDIF
    buff = buff+CHR(13)+CHR(10)
  ENDIF

  IF sw
    = FWRITE(f_out,buff)
  ENDIF
  sw = j
  buff = ""
ENDDO
= FCLOSE(n_f)

IF l_s < 0 .OR. lj
  buff = CHR(12)
ELSE
  buff = REPLICATE(CHR(10),l_s)
ENDIF

p_end=.T.
IF !EMPTY(p_type_n)
  buff = buff+P_Ctrl(pg_l, fnt, p_type_n, p_end)
ENDIF

= FWRITE(f_out,buff)
= FCLOSE(f_out)

RELEASE WINDOW Pr

IF EMPTY(w_sav)
  ACTIVATE SCREEN
ELSE
  ACTIVATE WINDOW (w_sav)
ENDIF

f_out = tmpo_path+"PRN_TMP.TXT "+p_u
FOR c_cnt = 1 TO n_c
  ! COPY /b &f_out > nul:
ENDFOR

RETURN


*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                   �
*�                蹍������ �ョ�皋 ぎ�祠キ�� (Windows).              �
*�                                                                   �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE P_Wind
PARAMETERS s_name
PRIVATE d_name, w_path

IF File_O(tmpo_path+"WORDPAD.TXT")
  DELETE FILE (tmpo_path+"WORDPAD.TXT")
ENDIF
  
d_name = tmpo_path+"WORDPAD.TXT"
DO Copy_D_W WITH s_name, d_name

w_path = "c:\progra~1\micros~1\office\winword"
USE (prmo_path+"O_PARMS") SHARED AGAIN ALIAS P_6302 IN 0
IF FSIZE("WORD_PATH", "P_6302") # 0
  IF .NOT. EMPTY(P_6302.WORD_PATH)
    w_path = ALLTRIM(P_6302.WORD_PATH)
  ENDIF
ENDIF
USE IN P_6302

w_path = w_path+" "+d_name

*IF Sale_Mode = 'MEGA'
  ! &w_path
*ELSE
*  ! WORDPAD &d_name
*ENDIF

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                   �
*�               蹍������ �ョ�皋 ぎ�祠キ�� (Win_Sale).              �
*�                                                                   �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE To_WinSale

PRIVATE s_sav

m.s_sav = SELECT()
SELECT 0
USE (base_path+"PRN_DOC") SHARED AGAIN ORDER TAG ARM_P ALIAS TO_W6322
IF .NOT. EMPTY(ARM_P) .OR. BOF() .OR. EOF()
  APPEND BLANK
ENDIF

REPLACE ARM_P	 WITH m.arm,	;
        DOC_TYPE WITH m.winType,;
        FIRM	 WITH m.winFirm,;
        DOC_NUM	 WITH m.winDoc,	;
        DOC_DATE WITH m.winDate,;
        REPEATE  WITH m.n_c
USE
SELECT (m.s_sav)
RETURN
