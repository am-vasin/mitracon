*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� P_Ctrl       ��о�｀砒┴ ��むォ ��瓱�           24.02.98 18:24:41 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�      蹍������ 筮爼�牀����� 祚��←闔薑� ESC-罐��腑� か� �爬�皀��.      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE P_Ctrl
PARAMETER p_len, fnt, p_type_n, p_end

*
*  p_len - か┃� 痰����肓
*  fnt - 皋� 葯�籵�, 0 - �°膈覃, 1 - condensed
*  p_type_n - ���ム ����瓱 (皋�� �爬�皀��) � PRNTYPES.DBF
*  p_end - ���ユ �襤��� ぎ�祠キ��?

PRIVATE s, start, nf, res, i, s_sav, tmpName

s_sav = SELECT()
res = ""

IF File_O(base_path+"PRNSETUP.DBF")
  SELECT 0
  USE (base_path+"PRNSETUP.DBF")
ELSE
  RETURN res  && �矚竄痰≪モ ��゛���, ���瘠��鉗�� 祚�.瓱�〓�� �爬�皀��
ENDIF

GO p_type_n
IF EOF()
  USE
  SELECT (s_sav)
  RETURN res && ���茯�� ｀�跏�� ���ム 皋�� �爬�皀��
ENDIF

tmpName = UPPER(ALLTRIM(PR_NAME))

DO CASE
CASE tmpName = "HP LASERJET 2100"
  IF p_end
    res = CHR(27)+"E"								&& �÷��
  ELSE
    res = CHR(27)+"E"								&& �÷��
    res = res+CHR(27)+IIF(EMPTY(LEFT(fnt,1)), ;
                          "&k0S", "&k2S")			&& �爬籵
    res = res+CHR(27)+IIF(EMPTY(SUBSTR(fnt,2,1)), ;
                          "&l0O", "&l1O")			&& �爬キ��罔�
  ENDIF
CASE tmpName = "EPSON"
  IF p_end
    IF Sale_Mode = 'MEGA'
      res = RESET
    ELSE
      res = CHR(27)+"@"								&& �÷��
    ENDIF
  ELSE
    IF Sale_Mode = 'MEGA'
      IF !EMPTY(Set_len) AND !EMPTY(p_len)
        res=Start+Set_len+CHR(p_len)+IIF(EMPTY(fnt), Set_Norm, Set_Cond)
      ELSE
        res=Start+IIF(EMPTY(fnt), Set_Norm, Set_Cond)
      ENDIF
    ELSE
      res = CHR(27)+"@"								&& �÷��
      res = res+IIF(EMPTY(LEFT(fnt,1)),   ;
                        CHR(18), CHR(15))			&& �爬籵
      IF p_len > 0
        res = res+CHR(27)+"C"+CHR(p_len)			&& ��┃� 痰����肓
      ENDIF
    ENDIF
  ENDIF
OTHERWISE
ENDCASE

USE
SELECT (s_sav)
RETURN res
