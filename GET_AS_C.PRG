*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�               踸ガ ��錢�� �� ÷���牀���┘ �� ��.                     �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Get_AS_C

PRIVATE get_path, mss, l_num, zak_arm, sel_rem

get_path = tmpo_path
IF .NOT. File_O(get_path+"DEBT_OUT.DBF")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �┴�� �� 磴�モ瘴 ���皋 ��� ��錢��."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

sel_rem = SELECT()
SELECT 0
USE (get_path+"DEBT_OUT")
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���� � ��錢��� �竅�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_rem)
  RETURN
ENDIF
SELECT 0
USE LIST_TIT ORDER TAG LST_ARM

IF EMPTY(DEBT_OUT.N_SP)  && �モ ���ム� 甎�瓷� - 甌Г�ガ!
  SELECT 0
  USE PARMS
  = FLOCK()
  l_num = LAST_LST+1
  REPLACE LAST_LST WITH l_num
  USE
  SELECT LIST_TIT
  APPEND BLANK
  REPLACE LST_ARM  WITH DEBT_OUT.N_S, ;
          LST_NUM  WITH l_num,  ;
          CR_DAT   WITH DATE(), ;
          LST_NAME WITH "���甌� �發カ� ����М�", ;
          WHO_CR   WITH user,   ;
          WHERE_CR WITH DEBT_OUT.N_S
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��Г�� 甎�甌� N "+ALLTRIM(STR(l_num,10))
  mss(3) = ""
  DO Out_Mess WITH 5, "mss"
ELSE
  IF .NOT. SEEK(DEBT_OUT.N_S+STR(DEBT_OUT.N_SP,6))
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�� ���ぅ� 甎�甌� � ���ム�� "+ALLTRIM(STR(DEBT_OUT.N_SP,10))
    mss(3) = ""
    DO Out_Mess WITH 5, "mss"
    SELECT LIST_TIT
    USE
    SELECT DEBT_OUT
    USE
    SELECT (sel_rem)
    RETURN
  ENDIF
  l_num = DEBT_OUT.N_SP
ENDIF
zak_arm = DEBT_OUT.N_S
SELECT 0
USE ORD_D
DELETE FILE (tmpo_path+"ORD_ZAK.DBF")
COPY STRUCTURE TO (tmpo_path+"ORD_ZAK.DBF")
USE (tmpo_path+"ORD_ZAK.DBF")
SELECT 0
USE STOCK ORDER TAG CODE ALIAS STO_COD AGAIN
SELECT 0
USE STOCK ORDER TAG NAME ALIAS STO_NAM AGAIN
SELECT 0
USE LIST_DET ORDER TAG L_NAME ALIAS LST_NAM
SELECT 0
USE LIST_DET ORDER TAG L_CODE ALIAS LST_COD AGAIN

SELECT DEBT_OUT
PRIVATE a_stru, n_fields, i, arr_in, arr_log
IF .NOT. File_O(log_path+"AS_LOG.DBF")   && �モ �窶���� - 甌Г�え�!
  n_fields = AFIELDS(a_stru)
  n_fields = n_fields+1
  DIMENSION a_stru(n_fields,4)
  FOR i = 1 TO n_fields-1
    a_stru(n_fields-i+1,1) = a_stru(n_fields-i,1)
    a_stru(n_fields-i+1,2) = a_stru(n_fields-i,2)
    a_stru(n_fields-i+1,3) = a_stru(n_fields-i,3)
    a_stru(n_fields-i+1,4) = a_stru(n_fields-i,4)
  ENDFOR
  a_stru(1,1) = "GET_DAT"
  a_stru(1,2) = "D"
  a_stru(1,3) = 8
  a_stru(1,4) = 0
  SELECT 0
  CREATE DBF (log_path+"AS_LOG") FROM ARRAY a_stru
  USE (log_path+"AS_LOG") EXCLUSIVE
  INDEX ON DTOS(GET_DAT) TAG HRON
  FOR i = 1 TO 1000
    APPEND BLANK
  ENDFOR
  USE
ENDIF
SELECT 0
USE (LOG_PATH+"AS_LOG") ORDER TAG HRON
SCATTER TO arr_log BLANK
arr_log(1) = DATE()

SELECT DEBT_OUT

PRIVATE k_lst, res_cod, lst_pos, q1, q2, q_d, new_b
k_lst = STR(l_num,6)

SCAN
  SCATTER TO arr_in
  FOR i = 2 TO ALEN(arr_log)
    arr_log(i) = arr_in(i-1)
  ENDFOR
  SELECT AS_LOG
  GO TOP
  GATHER FROM arr_log
  SELECT DEBT_OUT
  lst_pos = 0                       && ���ム ����瓱 � ����Д
  res_cod = STO_CODE

  IF res_cod # 0                    && �薀� �� 瓷��ぅ
    IF .NOT. SEEK(res_cod, "STO_COD")
      res_cod = 0
    ENDIF
  ENDIF

  IF EMPTY(res_cod)                 && 鉑皰� ���:
*    wait "鉑皰� ��� � ���牀瓮!" WINDOW &&&&&&&&&&&&&&&&
    IF SEEK(NAME+PREFIX,"STO_NAM")  && ���薀� �� 瓷��ぅ, ���� ��...
      res_cod = STO_NAM.CODE        &&    ��茫�!
      GO RECNO("STO_NAM") IN STO_COD
      IF SEEK(k_lst+STR(res_cod,7), ;
              "LST_COD")            &&    ... � � 甎�瓷�?
        lst_pos = RECNO("LST_COD")  &&       皰Ε ��茫�!!!
      ENDIF
    ENDIF
    IF lst_pos = 0                  && �甄� ぎ 瓱� ��� �� ��茫� � 甎�瓷�,

        IF SEEK(k_lst+NAME+PREFIX,  ;
              "LST_NAM")            &&    ���薀� �� ━キ�...
        lst_pos = RECNO("LST_NAM")  &&     ��茫�, �, 
        IF res_cod # 0              &&      メ�� ��錢┼瘴 ���,
          SELECT LST_NAM            &&
          REPLACE CODE WITH res_cod &&              �爬�錻襤�ガ!
          SELECT DEBT_OUT           &&
        ENDIF
      ENDIF
    ENDIF
  ELSE                              &&     ��� メ碎, � 皰���...
*    wait "�� �竅皰� ��� � ���牀瓮!" WINDOW &&&&&&&&&&&&&&&&
    IF SEEK(k_lst+STR(res_cod,7), ;
              "LST_COD")            &&    ���薀� � 甎�瓷� �� ��ゃ...
      lst_pos = RECNO("LST_COD")    &&      ��茫�!
*      wait "��茫� �� ��ゃ!" WINDOW &&&&&&&&&&&&&&&&
    ELSE                            &&    �� ��茫� �� ��ゃ - 
      IF SEEK(k_lst+NAME+PREFIX,  ;
              "LST_NAM")            &&        ���薀� �� ━キ�...
        lst_pos = RECNO("LST_NAM")  &&        ��茫�
        REPLACE LST_NAM.CODE WITH res_cod
*        wait "��茫� �� ━キ�!" WINDOW &&&&&&&&&&&&&&&&
*        WAIT STR(LST_NAM.CODE,10) WINDOW &&&&&&&&&&&
      ENDIF
    ENDIF
  ENDIF

  *
  *   ����腑痰〓, ��皰牀� °�� � ����Д � ぎ��
  IF lst_pos = 0
    q1  = 0
    q_d = 0
    SELECT LST_NAM
    IF DEBT_OUT.REQUIRED > 0
      APPEND BLANK
      REPLACE LIST_NUM WITH STR(l_num,6), ;
              CODE     WITH res_cod,      ;
              NAME     WITH IIF(res_cod#0, STO_COD.NAME, DEBT_OUT.NAME), ;
              PREFIX   WITH IIF(res_cod#0, STO_COD.PREFIX,  DEBT_OUT.PREFIX), ;
              ADDRESS  WITH IIF(res_cod#0, STO_COD.ADDRESS, " ")
    ENDIF
  ELSE
    GO lst_pos IN LST_NAM
    q1  = LST_NAM.QNT+LST_NAM.QNT_DUE
    q_d = LST_NAM.QNT_DUE
    SELECT LST_NAM
  ENDIF
  IF q1+DEBT_OUT.REQUIRED < 0
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"��Ж罔� "+ALLTRIM(DEBT_OUT.PREFIX)+" "+ALLTRIM(DEBT_OUT.NAME)+"."
    mss(3) = CHR(0)+"��┘�� �謔�モ瘴 ▲爿竄� ｀�跏�, 腑� �� �━ 腮甄�矚�. ����爬珮ガ!"
    mss(4) = ""
    DO Out_Mess WITH 7, "mss"
    SELECT DEBT_OUT
    LOOP
  ENDIF
  q2 = q1+DEBT_OUT.REQUIRED    && ��〓� 矗ァ礇��� ����腑痰〓
  new_b = QNT_REQ+IIF(res_cod # 0,  ;
          MAX(STO_COD.QNT, 0), 0)    && ��痰祚��� ��� ����腑痰〓
  new_b = MIN(q2, new_b)             && ��〓� Л�腑�┘ ÷���
  IF new_b # QNT_REQ                 && �牀�� ├�キ┼�瘡 - ぎ��錢��!
    SELECT ORD_ZAK
    APPEND BLANK
    REPLACE LIST_NUM WITH k_lst,    ;
            ORD_NUM  WITH LIST_TIT.ORD_NUM+1, ;
            CODE     WITH LST_NAM.CODE,       ;
            QNT      WITH new_b-LST_NAM.QNT_REQ
    SELECT STO_COD
    REPLACE QNT      WITH QNT-new_b+LST_NAM.QNT_REQ, ;
            QNT_SAVE WITH QNT_SAVE+new_b-LST_NAM.QNT_REQ
    SELECT LST_NAM
  ENDIF
  REPLACE QNT     WITH new_b,    ;
          QNT_REQ WITH new_b,    ;
          QNT_DUE WITH q2-new_b

  SELECT DEBT_OUT

ENDSCAN

IF RECCOUNT("ORD_ZAK") # 0
  USE ORD_T
  APPEND BLANK
  REPLACE LIST_NUM  WITH ORD_ZAK.LIST_NUM,  ;
          LIST_NAME WITH LIST_TIT.LST_NAME, ;
          ORD_NUM   WITH ORD_ZAK.ORD_NUM,   ;
          ORD_DATE  WITH DATE(),            ;
          ORD_TIME  WITH TIME(),            ;
          WHO       WITH user,              ;
          WHERE     WITH zak_arm
  USE
  SELECT ORD_ZAK
  USE ORD_D
  APPEND FROM (tmpo_path+"ORD_ZAK")
  USE
  SELECT LIST_TIT
  REPLACE ORD_NUM WITH ORD_NUM+1
*  IF new_ord
*    DO Prn_OrdN WITH LST_NUM, ORD_NUM
*  ELSE
    DO Prn_Ord WITH LST_NUM, ORD_NUM
*  ENDIF
  IF ORD_NUM > 900
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"｡��皋皀 ↓━��┘: 痲�爼�牀↓� ��錢�� N "+ALLTRIM(STR(ORD_NUM,3))+"."
    mss(3) = CHR(0)+"��ぅ釶�, �� ���━�モ� � 腑�� �� �爬゛└��瘴?"
    mss(4) = ""
    DO Out_Mess WITH 7,"mss"
  ENDIF
ENDIF

IF USED("DEBT_OUT")
  SELECT DEBT_OUT
  USE
ENDIF

IF USED("LIST_TIT")
  SELECT LIST_TIT
  USE
ENDIF

IF USED("ORD_ZAK")
  SELECT ORD_ZAK
  USE
ENDIF

IF USED("STO_COD")
  SELECT STO_COD
  USE
ENDIF

IF USED("STO_NAM")
  SELECT STO_NAM
  USE
ENDIF

IF USED("LST_COD")
  SELECT LST_COD
  USE
ENDIF

IF USED("LST_NAM")
  SELECT LST_NAM
  USE
ENDIF

IF USED("AS_LOG")
  SELECT AS_LOG
  USE
ENDIF

IF USED("ORD_D")
  SELECT ORD_D
  USE
ENDIF

SELECT (sel_rem)

DELETE FILE (tmpo_path+"ORD_ZAK.DBF")

RETURN



&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�          蹍罐ゃ�� �皖琺皋� ( 甌Г���� ) ����� - �竅硅茯�.            �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Use_Dummy

SELECT 0
IF .NOT. FILE_O(tmpo_path+"DUMMY.DBF")
  CREATE DBF (tmpo_path+"DUMMY") ( F C(1))
  APPEND BLANK
ELSE
  USE (tmpo_path+"DUMMY")
ENDIF
*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�             蹍������ �牀▲爲� ����腮� �����.                         �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE File_O
PARAMETERS f_nam

PRIVATE arr_w
RETURN adir(arr_w, f_nam) > 0
*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                      蹍������ �襪�腮 甌�♂キ��.                      �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Out_Mess
PARAMETERS c_sch, ind
*
* c_sch - ���ム 罌モ�〓� 痳ガ�
* ind   - ┃ぅ�� 甌�♂キ��
*
PRIVATE ln, l, w_sav, c_sav, alt, st_w, i1, i2, ind_w, j1, j2, i, s_d
IF TYPE(ind+"(1)") # "C"   && �� ��瘁│
  RETURN
ENDIF
*IF c_sch = 7
*  ! spp ��������
*ENDIF
s_d = SET("DEVICE") = "PRINTER"
SET DEVICE TO SCREEN
c_sav = SET("CURSOR") = "ON"
SET CURSOR OFF
ln = ALEN(&ind)            && 腮甄� 痰牀� � 甌�♂キ┬
DIMENSION ind_w(ln)

*
*                ���瓷 ��赳ム��皋�
*
PRIVATE alt_n, alt_lst
alt_n = 0
FOR l = 1 TO ln   && �牀甃�矗│�ガ ��甄イ���皀�讚� 痰��� 甌�♂キ�� �...
  st_w = &ind(l)
  IF LEFT(st_w,1) = CHR(0)
    st_w = PADC(SUBSTR(st_w,2),WCOLS("")-2)
  ENDIF
  ind_w(l) = ""   && �ムク�瘠��ガ �� � ��｀腮� ��瘁│ � �爬 轤��:
  DO WHILE .NOT. EMPTY(st_w)
    i1 = AT("[",st_w)   && �薀� ������ �腑爛き�� ��赳ム��皋��
    IF i1 # 0           && メ�� ��茫� 皰...
      ind_w(l) = ind_w(l)+LEFT(st_w,i1-1)
      st_w = SUBSTR(st_w,i1+1)
      i2 = AT("]",st_w)   && �薀� ���ユ 轤�� ��赳ム��皋��
      IF i2 # 0
        alt_n = alt_n+1
        DIMENSION alt_lst(alt_n,3)
        alt_lst(alt_n,1) = l
        alt_lst(alt_n,2) = LEN(ind_w(l))+1
        ind_w(l) = ind_w(l)+LEFT(st_w,i2-1)
        alt_lst(alt_n,3) = LEN(ind_w(l))
        st_w = SUBSTR(st_w,i2+1)
      ELSE
        ind_w(l) = ind_w(l)+st_w
        st_w = ""
      ENDIF
    ELSE
      ind_w(l) = ind_w(l)+st_w
      st_w = ""
    ENDIF
  ENDDO
ENDFOR
alt = alt_n > 1

*
*   �カ�ガ ����
*
w_sav = WOUTPUT()
*ACTIVATE SCREEN
l = WROWS()-ln-2
DEFINE WINDOW w_mess FROM l, 0 TO WROWS()-1, WCOLS()-1 COLOR SCHEME c_sch ;
                                SHADOW NONE
ACTIVATE WINDOW w_mess
@ 0,0 TO WROWS()-1, WCOLS()-1 DOUBLE
l = FLOOR((WCOLS()-IIF(alt,32,23))/2)
@ WROWS()-1, l SAY IIF(alt, " �襦ム�皀 �癶モ � ��Μ�皀 Enter", ;
                            " ��Μ�皀 �遏竡 ���※蓊 ")
FOR l = 1 TO ln
  @ l, 1 SAY ind_w(l)
ENDFOR
i1 = 0
IF alt              && メ�� 甌�♂キ┘ � ��赳ム��皋����...
  i1 = 1
  st_w = SCHEME(c_sch,2)
  DO WHILE .T.
    i1 = IIF(i1<1, alt_n, i1)
    i1 = IIF(i1>alt_n, 1, i1)
    @ alt_lst(i1,1),alt_lst(i1,2) FILL TO alt_lst(i1,1),alt_lst(i1,3) ;
          COLOR (st_w)
    i2 = INKEY(0,"M")
    IF i2 = 19 .OR. i2 = 4 .OR. i2 = 151
       @ alt_lst(i1,1),alt_lst(i1,2) FILL TO alt_lst(i1,1),alt_lst(i1,3) ;
            COLOR SCHEME c_sch
    ENDIF
    IF i2 = 28
      IF TYPE("hlp_idx") # "C"
        LOOP
      ENDIF
*      DO Hlp_Mess WITH hlp_idx
    ENDIF
    IF i2 = 19     && 痰爛��� ←ア�
      i1 = i1-1
    ENDIF
    IF i2 = 4      && 痰爛��� ����〓
      i1 = i1+1
    ENDIF
    IF i2 = 13     && Enter
      EXIT
    ENDIF
    IF i2 = 151    && �謌��
      j1 = MROW()
      j2 = MCOL()
      FOR i = 1 TO alt_n
        IF j1 = alt_lst(i,1) .AND. ;
           BETWEEN(j2,alt_lst(i,2),alt_lst(i,3))
          i1 = i
          KEYBOARD CHR(13)
          EXIT
        ENDIF
      ENDFOR
    ENDIF
  ENDDO
ELSE
  i1 = 0
  = INKEY(0,"M")
ENDIF
RELEASE WINDOW w_mess
IF .NOT. EMPTY(w_sav)
  ACTIVATE WINDOW (w_sav)
ENDIF
IF c_sav
  SET CURSOR ON
ENDIF
IF s_d
  SET DEVICE TO PRINTER
ENDIF
RETURN i1
*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�          蹍罐ゃ�� 甌Г���� ����.                                     �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Def_Win
PARAMETERS w_nam_1, w_nam_2, w_nam_3, ln, wd, m_tit, tit, foot
PRIVATE ln1, l, c, n, l_arr_t, l_arr_f

IF TYPE("tit(1)") # "C"
  l_arr_t = 0
ELSE
  l_arr_t = ALEN(tit)
  DIMENSION tit(l_arr_t)
ENDIF

IF TYPE("foot(1)") # "C"
  l_arr_f = 0
ELSE
  l_arr_f = ALEN(foot)
  DIMENSION foot(l_arr_f)
ENDIF

ln1 = ln+MAX(l_arr_t,1)+MAX(l_arr_f,1)+1

l = CEILING((WROWS("")-ln1)/2)
c = FLOOR((WCOLS("")-wd-4)/2)

DEFINE WINDOW (w_nam_1) FROM l, c TO l+ln1-1, c+wd+3 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW (w_nam_1)
@ 0, 0 SAY PADC(m_tit,WCOLS()) COLOR SCHEME 5

IF l_arr_t > 1
  FOR i = 1 TO l_arr_t-1
    @ i, 2 SAY PADC(tit(i), wd)
  ENDFOR
ENDIF

i = MAX(l_arr_t,1)
@ i, 1 TO i+ln+1, WCOLS()-2
IF l_arr_t # 0
  @ l_arr_t, 2 SAY PADC(tit(l_arr_t), wd,"�")
ENDIF

n = WROWS()-l_arr_f-1
IF l_arr_f # 0
  @ n+1, 2 SAY PADC(foot(1), wd,"�")
  FOR i = 2 TO l_arr_f
    @ i+n, 2 SAY PADC(foot(i), wd)
  ENDFOR
ENDIF

l = l+MAX(l_arr_t,1)+1
c = c+2
DEFINE WINDOW (w_nam_2) FROM l, c TO l+ln-1, c+wd-1 NONE COLOR SCHEME 13
DEFINE WINDOW (w_nam_3) FROM l-3, c-2 TO l+ln, c+wd+20   ;
                           NONE COLOR SCHEME 13
RETURN
*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�             蹍������ 甌Г���� ����.                                  �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE D_W
PARAMETERS w_nam, ln, wd, tit, board
*
*  w_nam - ━� ����;
*  ln    - か┃� ����;
*  wd    - 荐爬�� ����;
*  tit   - ������〓�;
*  board - �����: 0 - �� 爬甌��碎, 1 - �え��爿��, 2 - あ�����
*
PRIVATE b_w, c, l
IF TYPE("board") = "N"
  b_w = board
ELSE
  b_w = 1
ENDIF
l = CEILING((WROWS("")-ln)/2)
c = FLOOR  ((WCOLS("")-wd)/2)

DEFINE WINDOW (w_nam) FROM l, c TO l+ln-1, c+wd-1 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW (w_nam)
@ 0, 0 SAY PADC(tit,WCOLS()) COLOR SCHEME 5
DO CASE
CASE b_w = 1
  @ 1, 1 TO WROWS()-1, WCOLS()-2
CASE b_w = 2
  @ 1, 1 TO WROWS()-1, WCOLS()-2 DOUBLE
ENDCASE
RETURN
*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                   蹍������ ��ぃ�皰→� �爬�皀��.                      �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Ini_Prn
PARAMETERS dc_n, p_l, l_f, n_c, fnt
*
*  dc_n - ━� ぎ�祠キ��;
*  p_l  - か┃� 痰����肓;
*  l_f  - �ア�� ����;
*  n_c  - 腮甄� ���┤;
*  fnt  - 葯�籵.
*

PRIVATE i, sel_rem, p_file, w1, w2, w3, w4, w5

IF TYPE("dc_n") = "C"
  w1 = dc_n
ELSE
  w1 = "<< �ェ ��Б���� >>"
ENDIF

IF TYPE("p_l") = "N"
  w2 = p_l
ELSE
  w2 = 66
ENDIF

IF TYPE("l_f") = "N"
  w3 = l_f
ELSE
  w3 = 0
ENDIF

IF TYPE("n_c") = "N"
  w4 = MAX(n_c,1)
ELSE
  w4 = 1
ENDIF

IF TYPE("fnt") = "C"
  w5 = fnt
ELSE
  w5 = " "
ENDIF

sel_rem = SELECT()
SELECT 0

IF .NOT. FILE(prmo_path+"PRINT.DBF")
  DELETE FILE (prmo_path+"PRINT.CDX")
  CREATE DBF (prmo_path+"PRINT")  ;
             ( PRN_DATE    D( 8), ;
               PRN_TIME    C( 8), ;
               DOC_NAME    C(40), ;
               PG_LENG     N( 3), ;
               COPY_NUM    N( 2), ;
               L_FIELD     N( 2), ;
               FONT        C( 2)  )
  INDEX ON DTOS(PRN_DATE)+PRN_TIME TAG HRON
  FOR i = 1 TO 10
    APPEND BLANK
  ENDFOR
ELSE
  USE (prmo_path+"PRINT.DBF") ORDER TAG HRON
ENDIF
GO TOP

REPLACE PRN_DATE WITH DATE(), ;
        PRN_TIME WITH TIME(), ;
        DOC_NAME WITH w1,     ;
        PG_LENG  WITH w2, ;
        COPY_NUM WITH w4, ;
        L_FIELD  WITH w3, ;
        FONT     WITH w5
p_file = tmpo_path+"PRINT_"+RIGHT(STR(1000+RECNO(),4),2)+".TXT"
SET PRINTER TO (p_file)
SET DEVICE TO PRINTER
??? CHR(27)+"@"

SELECT (sel_rem)

RETURN
*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                   �
*�               蹍������ �ョ�皋/�牀甃�矗� ぎ�祠キ��.               �
*�                                                                   �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Print_D
PARAMETERS ext_menu, f_nam, d_nm, pg_l, flt, fnt, ncp

PRIVATE pic1, pic2, pic3, pic4, w_sav
pic1 = "���� 甄ア� __      ���-〓 蹣Д���閠�� __"
pic2 = "�メ� ぎ�祠キ�   ;�����キ�"                   && +8 (@*RH)
pic3 = "�矗���肓 � ___ �� ___"
pic4 = " �ョ�碎 Ctrl-W ; 蹍甃�矗 ; ���� ; �皖���碎瘴 Esc " && +8 (@*HT)

PRIVATE ln, wd, l1, c1

w_sav = WOUTPUT()
wd = MAX(LEN(ALLTRIM(d_nm)),LEN(pic1),LEN(pic2)+8,LEN(pic3),LEN(pic4)+8)+6
ln = 12
DO D_W WITH "Prnt_D", ln, wd, ALLTRIM(d_nm), 1
PRIVATE c1, c2, c3, c4, c5, c6, p_s, p_e, l_f, n_c, trm, frg
pic1 = " �"+PADC(pic1,WCOLS()-4)+"� "
pic3 = " �"+PADC(pic3,WCOLS()-4)+"� "
@ 3, 0 SAY pic1
@ 7, 0 SAY pic3
p_s = 0
p_e = 0
frg = 1
trm = 4
l_f = flt
n_c = ncp
c1  = AT("__",pic1)-1
c2  = AT("__",pic1,2)-1
c3  = FLOOR((WCOLS()-LEN(pic2)-8)/2)
c4  = AT("___",pic3)-1
c5  = AT("___",pic3,2)-1
c6  = FLOOR((WCOLS()-LEN(pic4)-8)/2)

@ 9, c6 GET trm PICTURE "@*HT \!"+pic4
@ 3, c1 GET l_f PICTURE "@Z 99" VALID V_Left()
@ 3, c2 GET n_c PICTURE "@Z 99" VALID V_Copy()
@ 5, c3 GET frg PICTURE "@*RH "+pic2 VALID V_Frag()
@ 7, c4 GET p_s PICTURE "@Z 999" WHEN frg=2 VALID V_First()
@ 7, c5 GET p_e PICTURE "@Z 999" WHEN frg=2 VALID V_Last()

READ CYCLE VALID V_Read()

RELEASE WINDOW Prnt_D

l_f = flt
*n_c = ncp
DO CASE
CASE trm = 1   && �ョ�碎
  DO Prnt_Txt WITH f_nam, pg_l, l_f, n_c, p_s, p_e, fnt
CASE trm = 2   && 蹍甃�矗
  DO View_Txt WITH f_nam, d_nm, IIF(pg_l=0, 66, pg_l), l_f, fnt
CASE trm = 3   && ����瘡 � ����
  DO Txt_To_F WITH f_nam
ENDCASE

IF EMPTY(w_sav)
*  ACTIVATE SCREEN
ELSE
  ACTIVATE WINDOW (w_sav)
ENDIF

RETURN

*************************************************************************
*
*     蹍▲爲� ���※�讚�痰� ���ォ.
*
PROCEDURE V_Left

PRIVATE s, mss
s = READKEY() % 256
IF s = 12
  RETURN .T.
ENDIF

IF l_f < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����� ���ォ � �爬牀ぅ �� °��モ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*************************************************************************
*
*     蹍▲爲� ���※�讚�痰� 腮甄� ���┤.
*
PROCEDURE V_Copy

PRIVATE s, mss
s = READKEY() % 256
IF s = 12
  RETURN .T.
ENDIF

IF n_c < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ �┴���� 〓К�Ν�痰� ���ョ���碎 痰��讓� ���┤!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF n_c = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�甄� �� �� 絎皋皀 �ョ���碎 ぎ�祠キ�, �� �磴爬皀 - ��Μ�皀 Esc!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*************************************************************************
*
*     蹍▲爲� ����腮� 籥���キ��.
*
PROCEDURE V_Frag

PRIVATE s, mss
s = READKEY() % 256
IF s = 12
  RETURN .T.
ENDIF

IF frg = 1
  p_s = 0
  p_e = 0
  SHOW GET p_s
  SHOW GET p_e
ENDIF

RETURN .T.

*************************************************************************
*
*     蹍▲爲� ���ム� �����讚�� 痰����肓.
*
PROCEDURE V_First

PRIVATE s, mss
s = READKEY() % 256
IF s = 12
  RETURN .T.
ENDIF
IF frg = 1
  RETURN .T.
ENDIF

IF p_s <= 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����� ���ム�� 痰����� � �爬牀ぅ �� °��モ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.


*************************************************************************
*
*     蹍▲爲� ���ム� ���ョ��� 痰����肓.
*
PROCEDURE V_Last

PRIVATE s, mss
s = READKEY() % 256
IF s = 12
  RETURN .T.
ENDIF
IF frg = 1
  RETURN .T.
ENDIF

IF p_e <= 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����� ���ム�� 痰����� � �爬牀ぅ �� °��モ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF p_e < p_s
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����� ┃皀燿���� 痰����� � �爬牀ぅ �� °��モ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*************************************************************************
*
*     蹍▲爲� �諷��� ├ ゛����.
*
PROCEDURE V_Read

PRIVATE s, mss
s = READKEY() % 256
IF s = 12 .AND. READKEY(1) = 1
  trm = 4
  RETURN .T.
ENDIF

IF trm = 1
  IF .NOT. V_Left()
    RETURN .F.
  ENDIF
  IF .NOT. V_Copy()
    RETURN .F.
  ENDIF
  IF .NOT. V_Frag()
    RETURN .F.
  ENDIF
  IF .NOT. V_First()
    RETURN .F.
  ENDIF
  IF .NOT. V_Last()
    RETURN .F.
  ENDIF
ENDIF

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                   �
*�                     蹍������ �ョ�皋 ぎ�祠キ��.                   �
*�                                                                   �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Prnt_Txt
PARAMETERS f_nam, pg_l, l_f, n_c, p_s, p_e, fnt

PRIVATE buff, pag_no, i, j, n, c_cnt, n_f, sw

PRIVATE l1, c1, l2, c2, w_sav, ff, s
ff = CHR(12)
l2 = 8
c2 = 36
w_sav = WOUTPUT()
DO D_W WITH "Pr", l2, c2, "�ョ�碎 ぎ�祠キ��", 1

@ 3, 4 SAY "����� N"
@ 3,18 SAY "�矗����� N"
c1 = 12
c2 = 29

SET PRINTER TO PRN:
*SET PRINTER TO TMP_PRN.TXT

buff = CHR(27)+"@"+CHR(27)+"C"+CHR(pg_l)+CHR(27)+"l"+CHR(l_f)

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
    buff = FREAD(n_f, 100)
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

buff = CHR(27)+"@"
n = LEN(buff)
FOR i = 1 TO n
  ??? SUBSTR(buff,i,1)
ENDFOR

RELEASE WINDOW Pr

IF EMPTY(w_sav)
*  ACTIVATE SCREEN
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

PRIVATE l1, c1, s, new_f, w_sav
new_f = SPACE(40)
l2 = 6
c2 = 18+LEN(new_f)
w_sav = WOUTPUT()
DO D_W WITH "Sav_F", l2, c2, "����瘡 ぎ�祠キ�� � ����", 1
@ WROWS()-1, 2 SAY PADC(" Esc - �皖���碎瘴 ", WCOLS()-4, "�")
@ 3, 4 SAY "��� �����" GET new_f VALID Emp_Nam()
READ

IF MOD(READKEY(),256) # 12
  DO Wt_Mess WITH "����瘡..."
  new_f = ALLTRIM(new_f)
  COPY FILE (f_nam) TO (new_f)
  DO Wt_Mess
ENDIF

RELEASE WINDOW Sav_F

IF EMPTY(w_sav)
*  ACTIVATE SCREEN
ELSE
  ACTIVATE WINDOW (w_sav)
ENDIF

RETURN

************************************************************************
PROCEDURE Emp_Nam

IF MOD(READKEY(),256) = 12 .OR. .NOT. EMPTY(new_f)
  RETURN .T.
ENDIF

PRIVATE mss(3)
mss(1) = ""
mss(2) = CHR(0)+"�皰 艪� �� ���� �����?"
mss(3) = ""
RETURN .F.
*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                  蹍������ �ョ�皋 ��錢�� �� 瓷���.                    �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Prn_Ord

PARAMETERS d_n, add_n
*
*  d_n   - ���ム 甎�瓷� ( 腮甄� );
*  add_n - ���ム ��錢�� ( 腮甄� ).
*

PRIVATE p_ln, fnt, n_cp, lft, n, i, j, stw, t_p, e_p, tmp_f
PRIVATE sel_rem, sb
DIMENSION sb(7,2)

sel_rem = SELECT()

SELECT 0

USE ORD_T ORDER TAG DOC_NUM AGAIN ALIAS TMP000
SEEK STR(d_n,6)+STR(add_n,3)
IF .NOT. FOUND()
  USE
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���祠キ� �� ���ぅ� "+ALLTRIM(STR(d_n,6))+"/"+ALLTRIM(STR(add_n,3))
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (sel_rem)
  RETURN
ENDIF
sb(1,1) = "{Ord_No  }"
sb(1,2) = ALLTRIM(STR(d_n,6))+"/"+ALLTRIM(STR(add_n,3))
sb(1,2) = PADR(sb(1,2),LEN(sb(1,1)))

sb(2,1) = "{Date    }"
sb(2,2) = TRANSFORM(ORD_DATE,"@D")
sb(2,2) = PADR(sb(2,2),LEN(sb(2,1)))

sb(3,1) = "{Ord_Name                      }"

sb(4,1) = "{C}"
sb(4,2) = "   "

sb(5,1) = "{Arm           }"
sb(5,2) = WHERE

sb(6,1) = "{Salesman                      }"
sb(6,2) = WHO

sb(7,1) = "{Time  }"
sb(7,2) = ORD_TIME

sb(3,2) = LIST_NAME
sb(3,2) = PADR(sb(3,2),LEN(sb(3,1)))

IF sb(6,2) = 0
  sb(6,2) = "��� □カ!"
ELSE
  USE USERS ORDER TAG USER_CODE AGAIN ALIAS TMP000
  SEEK sb(6,2)
  IF FOUND()
    sb(6,2) = ALLTRIM(USER_NAME)
  ELSE
    sb(6,2) = ""
  ENDIF
ENDIF
sb(6,2) = PADR(sb(6,2),LEN(sb(6,1)))

USE ARM_LST AGAIN ALIAS TMP000
LOCATE FOR sb(5,2) = ARM_C
IF FOUND()
  sb(5,2) = ARM_N
ELSE
  sb(5,2) = ""
ENDIF
sb(5,2) = PADR(sb(5,2),LEN(sb(5,1)))

USE DOC_FORM
LOCATE FOR "ORDER" = ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD

n = MEMLINES(DOC_H)
DIMENSION t_p(n)
FOR i = 1 TO n
  t_p(i) = MLINE(DOC_H,i)
ENDFOR

n = MEMLINES(DOC_F)
DIMENSION e_p(n)
FOR i = 1 TO n
  e_p(i) = MLINE(DOC_F,i)
ENDFOR
USE

tmp_f = tmpo_path+SYS(3)
CREATE DBF (tmp_f) (NAME C(29), ADDRESS C( 9), QNT N( 6))
USE (tmp_f) ALIAS P_TMP
INDEX ON ADDRESS TAG ADDRESS

SELECT 0
USE STOCK ORDER TAG CODE AGAIN ALIAS S000

SELECT 0
USE ORD_D ORDER TAG DOC_NUM AGAIN ALIAS TMP000
SET RELATION TO CODE INTO S000
SEEK STR(d_n,6)+STR(add_n,3)
SCAN REST WHILE LIST_NUM = STR(d_n,6) .AND. ORD_NUM = add_n
  SELECT P_TMP
  APPEND BLANK
  REPLACE NAME    WITH S000.PREFIX+S000.NAME, ;
          ADDRESS WITH S000.ADDRESS+IIF(EMPTY(S000.MEMO_ST), " ", "+"), ;
          QNT     WITH TMP000.QNT
  SELECT TMP000
ENDSCAN
USE
SELECT S000
USE

DO Ini_Prn WITH "��錢�� �� 瓷��� N "+sb(1,2), p_ln, lft, n_cp, fnt

FOR i = 1 TO ALEN(t_p)
  stw = t_p(i)
  FOR j = 1 TO 7
    stw = STRTRAN(stw, sb(j,1), sb(j,2))
  ENDFOR
  IF i = 1
    @ PROW(), PCOL() SAY stw
  ELSE
    @ PROW()+1, 0 SAY stw
  ENDIF
ENDFOR

SELECT P_TMP
SCAN
  stw = ADDRESS+NAME+STR(QNT,6)+"|"+LEFT(NAME,23)+STR(QNT,6)
  @ PROW()+1, 0 SAY stw
ENDSCAN
USE
DELETE FILE (tmp_f+".dbf")
DELETE FILE (tmp_f+".cdx")

FOR i = 1 TO ALEN(e_p)
  stw = e_p(i)
  FOR j = 1 TO 7
    stw = STRTRAN(stw, sb(j,1), sb(j,2))
  ENDFOR
  @ PROW()+1, 0 SAY stw
ENDFOR
*EJECT
DO Term_Prn WITH ""

SELECT (sel_rem)
RETURN
*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                   �
*�                 蹍������ ��▲琥キ�� �ョ�皋.                      �
*�                                                                   �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Term_Prn
PARAMETERS ext_menu

PRIVATE sel_rem

SET DEVICE TO SCREEN
SET PRINTER TO PRN:

sel_rem = SELECT()
SELECT PRINT

PRIVATE f_nam, d_nm, pg_l, flt, fnt, ncp

f_nam = tmpo_path+"PRINT_"+RIGHT(STR(1000+RECNO(),4),2)+".TXT"
d_nm = DOC_NAME
pg_l = PG_LENG
ncp = COPY_NUM
flt = L_FIELD
fnt = FONT

USE
SELECT (sel_rem)

DO Print_D WITH ext_menu, f_nam, d_nm, pg_l, flt, fnt, ncp

RETURN
*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                   �
*�                   蹍������ �牀甃�矗� �����.                      �
*�                                                                   �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE View_txt
PARAMETER f_name, d_titl, prn_p_len, lft_f, fnt
*
*   f_name    - ������ ━� ����� � ��瘉�爛�┘� �, メ�� 矗ァ礇矚� � �竄ガ;
*   d_titl    - ━� ぎ�祠キ��;
*   prn_p_len - か┃� 痰����肓;
*   lft_f     - �ア�� ����;
*   fnt       - 葯�籵.
*
PRIVATE nf, buf, fl_p, j, i, k, n_pgs, scale_h, scale_v, sav_sel

PRIVATE font    && ������〓� ぎ�祠キ�� � 筮��: 0 - ��爼.,
                        &&       1 - condenced
sav_sel = SELECT()

IF TYPE("fnt") = "C"
  font = IIF(fnt=" ", 0, 1)
ELSE
  font = 0
ENDIF

nf = FOPEN(f_name)

IF FILE (tmpo_path+"DOC_STRU.DBF")
  DELETE FILE (tmpo_path+"DOC_STRU.DBF")
ENDIF

CREATE DBF (tmpo_path+"DOC_STRU") ( P_POS N(10) )

APPEND BLANK

k = 0
DO WHILE .NOT. FEOF(nf)
  buf = FREAD(nf, 1024)
  j = 1
  i = AT(CHR(12),buf,j)
  DO WHILE i # 0
    j = j+1
    APPEND BLANK
    REPLACE P_POS WITH k*1024+i+1
    i = AT(CHR(12),buf,j)
  ENDDO
  k = k+1
ENDDO

n_pgs = RECCOUNT()
IF RIGHT(buf,2) = CHR(12)+CHR(13)
  DELETE
  n_pgs = n_pgs-1
ENDIF
PRIVATE ln_w, l_w, c_w, wd_w, prn_p_wd

prn_p_wd = IIF(font = 0, 80, 132)
ln_w = MIN(WROWS("")-5, prn_p_len+4)
wd_w = MIN(WCOLS("")-4, prn_p_wd+4)

l_w = CEILING((WROWS("")-ln_w)/2)
c_w = FLOOR((WCOLS("")-wd_w)/2)

DEFINE WINDOW View_M FROM l_w, c_w TO l_w+ln_w-1, c_w+wd_w-1    ;
                                      NONE SHADOW COLOR SCHEME 13
DEFINE WINDOW View_E FROM l_w+2, c_w+2 TO l_w+ln_w-3, c_w+wd_w-3   ;
                                      NONE COLOR SCHEME 13
*DEFINE WINDOW View_I FROM l_w+1, c_w+2 TO l_w+prn_p_len+1, c_w+prn_p_wd+1   ;
*                                      NONE SHADOW COLOR SCHEME 10
DEFINE WINDOW View_I FROM 0, 0 TO prn_p_len-1, prn_p_wd-1   ;
                                      NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW View_M
@ 0, 0 SAY PADC(ALLTRIM(d_titl),WCOLS()) COLOR SCHEME 5
@ 1, 1 TO WROWS()-2, WCOLS()-2
k = "PgDn/PgUp - 甄イ./�爛�. 痰������, 痰爛��� - �ムガラ. 皀�痰�, Esc - �諷��."
@ WROWS()-1, 0 SAY PADC(k, WCOLS())
ACTIVATE WINDOW View_E
ACTIVATE WINDOW View_I IN WINDOW View_E
scale_h = ""
k = 1
DO WHILE LEN(scale_h) < WCOLS()
  scale_h = scale_h+"��������"+RIGHT(STR(k),1)
  k = k+1
ENDDO
DIMENSION scale_v(WROWS())
FOR k = 1 TO WROWS()
  IF MOD(k,5) # 0
    scale_v(k) = " �"
  ELSE
    IF MOD(k,10) # 0
      scale_v(k) = " "
    ELSE
      scale_v(k) = LEFT(STR(k,3),2)
    ENDIF
  ENDIF
ENDFOR
fl_p = 1
DO Prep_Pg
PRIVATE s, crsr
crsr = SET("CURSOR") = "ON"
SET CURSOR OFF

DO WHILE .T.
  s = INKEY(0,"M")
  DO CASE
  CASE s = 27  && Esc
    RELEASE WINDOW View_M
    RELEASE WINDOW View_E
    RELEASE WINDOW View_I
    = FCLOSE(nf)
    SELECT DOC_STRU
    USE
    SELECT (sav_sel)
    IF crsr
      SET CURSOR ON
    ENDIF
    EXIT
  CASE s = 5   && Up
    IF WLROW("View_I") # WLROW("View_E")
      MOVE WINDOW View_I BY 1, 0
      ACTIVATE WINDOW View_M
      SCROLL 2, 0, 1+WROWS("View_E"), 1, -1
      @ 2, 0 SAY scale_V(WLROW("View_E")-WLROW("View_I")+1) COLOR SCHEME 13
      ACTIVATE WINDOW View_I
    ENDIF
  CASE s = 24  && Dn
    IF WLROW("View_I")+WROWS("View_I") # WLROW("View_E")+WROWS("View_E")
      MOVE WINDOW View_I BY -1, 0
      ACTIVATE WINDOW View_M
      SCROLL 2, 0, 1+WROWS("View_E"), 1, 1
      @ 2+WROWS("View_E")-1, 0 SAY  ;
          scale_v(WLROW("View_E")-WLROW("View_I")+WROWS("View_E")) COLOR SCHEME 13
      ACTIVATE WINDOW View_I
    ENDIF
  CASE s = 19  && Left
    IF WLCOL("View_I") # WLCOL("View_E")
      MOVE WINDOW View_I BY 0, 1
      ACTIVATE WINDOW View_M
      @ 1, 2 SAY SUBSTR(scale_h,WLCOL("View_E")-WLCOL("View_I")+1,WCOLS("View_E"))  ;
                          COLOR SCHEME 13
      ACTIVATE WINDOW View_I
    ENDIF
  CASE s = 4   && Right
    IF WLCOL("View_I")+WCOLS("View_I") # WLCOL("View_E")+WCOLS("View_E")
      MOVE WINDOW View_I BY 0, -1
      ACTIVATE WINDOW View_M
      @ 1, 2 SAY SUBSTR(scale_h,WLCOL("View_E")-WLCOL("View_I")+1,WCOLS("View_E"))  ;
                      COLOR SCHEME 13
      ACTIVATE WINDOW View_I
    ENDIF
  CASE s = 18  && PgUp
    IF fl_p # 1
      fl_p = fl_p-1
      DO Prep_Pg
    ENDIF 
  CASE s = 3   && PgDn
    IF fl_p # n_pgs
      fl_p = fl_p+1
      DO Prep_Pg
    ENDIF 
  ENDCASE
ENDDO

SELECT (sav_sel)
RETURN


PROCEDURE Prep_Pg
PRIVATE s, p, i

CLEAR
GO fl_p
= FSEEK(nf,P_POS)
p = 0
s = FGETS(nf)

DO WHILE .T.    && .NOT. FEOF(nf)
  IF p > WROWS()-1
    EXIT
  ENDIF
  IF RIGHT(s,1) = CHR(12)
    s = LEFT(s,LEN(s)-1)
    @ p, lft_f SAY LEFT(s,WCOLS()-lft_f)
    EXIT
  ENDIF
  @ p, lft_f SAY LEFT(s,WCOLS()-lft_f)
  p = p+1
  IF FEOF(nf)
    EXIT
  ENDIF
  s = FGETS(nf)
ENDDO

ACTIVATE WINDOW View_M
i = PADR("�矗. "+ALLTRIM(STR(fl_p,5))+   ;
                        "/"+ALLTRIM(STR(n_pgs,5)), 12)
@ 0, WCOLS()-14 SAY i COLOR SCHEME 5
@ 1, 1 SAY "�" COLOR SCHEME 13
@ 1, 2 SAY LEFT(scale_h,WCOLS("View_E")) COLOR SCHEME 13
FOR i = 1 TO WROWS("View_E")
  @ 1+i, 0 SAY scale_v(i) COLOR SCHEME 13
ENDFOR

ACTIVATE WINDOW View_I
MOVE WINDOW View_I TO 0, 0

RETURN
*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                 蹍罐ゃ�� �襪�腮 ��痰�→�.                            �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Wt_Mess
PARAMETER mes  && 甌�∴キ┘

PRIVATE mes_w, l1, c1, l2, c2, i

IF WEXIST("Wt_Mess")
  RELEASE WINDOW Wt_Mess
  RETURN
ENDIF
mes_w = ""
IF TYPE("mes") = "C"
  mes_w = mes
ENDIF

l2 = IIF(EMPTY(mes_w), 5, 6)
l1 = FLOOR((WROWS("")-l2)/2)
l2 = l1+l2-1

c2 = MAX(LEN(mes_w),12)+8
c1 = FLOOR((WCOLS("")-c2)/2)
c2 = c1+c2-1

DEFINE WINDOW Wt_Mess FROM l1, c1 TO l2, c2 NONE SHADOW COLOR SCHEME 5
ACTIVATE WINDOW Wt_Mess

i = 2
IF .NOT. EMPTY(mes_w)
  @ i, 0 SAY PADC(mes_w,WCOLS())
  i = i+1
ENDIF
@ i, 0 SAY PADC("�ぅ�-�...",WCOLS())
RETURN
