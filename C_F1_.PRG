*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� C_F1_        ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                    ������珥�覃 �諷�� ├ ���� 筥爼�.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 15.01.1999 様�
PROCEDURE C_F1_
PARAMETER ex_proc

IF READKEY() % 256 = 12
  ON KEY LABEL F1
  RETURN .T.
ENDIF
PRIVATE s_sav, mss
s_sav = SELECT()
SELECT 0
IF .NOT. EMPTY(f_c000)
  f_c000 = CHRTRAN(f_c000,shab1, shab2)
  SHOW GET f_c000
  USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F0_9115 AGAIN
  IF .NOT. SEEK(f_c000)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�� �アム�� 礫����� ��� �爛く爬閧��!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    SELECT (s_sav)
    RETURN .F.
  ENDIF
  f_n000 = B_NAME
  SHOW GET f_n000
  * && NEW 22.12.1999 begin
  lt_wchk = LET_WCHK     && !!! REQUIRES FIRMS.LET_WCHK field !!!
  df_wchk = DEF_WCHK     && !!! REQUIRES FIRMS.DEF_WCHK field !!!
  w_check = df_wchk
  IF WONTOP('Corr_Tit')  && update w_check checkbox only if it is present
    SHOW GET w_check
  ENDIF
  * && NEW 22.12.1999 end
  IF .NOT. EMPTY(DATE_OFF)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�皰 �爛く爬閧┘ 磴��キ�! �� ��痰�│�モ� �� 瓣�ガ �襦�爛? [ �モ ] [ �� ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 1
      USE
      SELECT (s_sav)
      RETURN .F.
    ENDIF
  ENDIF
  IF f_c_old # f_c000
    b_c000 = "   "
    b_n000 = SPACE(16)
    SHOW GET b_n000
  ENDIF
  USE
  SELECT (s_sav)
  ON KEY LABEL F1
  IF .NOT. EMPTY(ex_proc)
    DO (ex_proc)
  ENDIF
  RETURN .T.
ELSE
  b_c000 = "   "
  b_n000 = SPACE(16)
  f_n000 = SPACE(16)
  SHOW GET b_n000
  SHOW GET f_n000
  SELECT (s_sav)
  ON KEY LABEL F1
  IF .NOT. EMPTY(ex_proc)
    DO (ex_proc)
  ENDIF
  RETURN .T.
ENDIF
