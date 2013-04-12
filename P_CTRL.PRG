*������������������������������������������������������������������������ͻ
*� ���� P_Ctrl       ���ࠡ��稪 ���३ ��ᨭ           24.02.98 18:24:41 �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�      �ணࠬ�� �ନ஢���� �ࠢ����� ESC-楯�祪 ��� �ਭ��.      �
*�                                                                        �
*������������������������������������������������������������������������ͼ
PROCEDURE P_Ctrl
PARAMETER p_len, fnt, p_type_n, p_end

*
*  p_len - ����� ��࠭���
*  fnt - ⨯ ����, 0 - �����, 1 - condensed
*  p_type_n - ����� ����� (⨯� �ਭ��) � PRNTYPES.DBF
*  p_end - ����� �뢮�� ���㬥��?

PRIVATE s, start, nf, res, i, s_sav, tmpName

s_sav = SELECT()
res = ""

IF File_O(base_path+"PRNSETUP.DBF")
  SELECT 0
  USE (base_path+"PRNSETUP.DBF")
ELSE
  RETURN res  && ��������� ⠡���, ����뢠��� ��.ᨬ���� �ਭ��
ENDIF

GO p_type_n
IF EOF()
  USE
  SELECT (s_sav)
  RETURN res && ���誮� ����让 ����� ⨯� �ਭ��
ENDIF

tmpName = UPPER(ALLTRIM(PR_NAME))

DO CASE
CASE tmpName = "HP LASERJET 2100"
  IF p_end
    res = CHR(27)+"E"								&& ����
  ELSE
    res = CHR(27)+"E"								&& ����
    res = res+CHR(27)+IIF(EMPTY(LEFT(fnt,1)), ;
                          "&k0S", "&k2S")			&& ����
    res = res+CHR(27)+IIF(EMPTY(SUBSTR(fnt,2,1)), ;
                          "&l0O", "&l1O")			&& �ਥ����
  ENDIF
CASE tmpName = "EPSON"
  IF p_end
    IF Sale_Mode = 'MEGA'
      res = RESET
    ELSE
      res = CHR(27)+"@"								&& ����
    ENDIF
  ELSE
    IF Sale_Mode = 'MEGA'
      IF !EMPTY(Set_len) AND !EMPTY(p_len)
        res=Start+Set_len+CHR(p_len)+IIF(EMPTY(fnt), Set_Norm, Set_Cond)
      ELSE
        res=Start+IIF(EMPTY(fnt), Set_Norm, Set_Cond)
      ENDIF
    ELSE
      res = CHR(27)+"@"								&& ����
      res = res+IIF(EMPTY(LEFT(fnt,1)),   ;
                        CHR(18), CHR(15))			&& ����
      IF p_len > 0
        res = res+CHR(27)+"C"+CHR(p_len)			&& ����� ��࠭���
      ENDIF
    ENDIF
  ENDIF
OTHERWISE
ENDCASE

USE
SELECT (s_sav)
RETURN res