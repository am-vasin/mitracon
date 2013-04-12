*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Debug_R      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                   �ணࠬ�� ॣ����樨 �࠭���権.                    �
*�                                                                        �
*����������������������������������������������������������� 04/03/1999 �ͼ
PROCEDURE Debug_R
PARAMETERS als, is_add, vl, p_name, p_line, note

* als    - �����;
* is_add - �ਧ��� ����������/���४��;
* vl     - �㬬�;
* p_name - ��� �ணࠬ��;
* p_line - ����� ��ப� �ணࠬ��;
* note   - �ਬ�砭�� (�����.).

PRIVATE s_sav, db_name, i

IF .NOT. USED("DEBUG_R")
  RETURN
ENDIF

s_sav = SELECT()
SELECT DEBUG_R

GO TOP
IF BOF() .OR. EOF()
  SELECT (s_sav)
  RETURN
ENDIF

db_name = DBF(als)
i = RAT("\", db_name)
IF i # 0
  db_name = SUBSTR(db_name, i+1)
ENDIF

REPLACE D_DATE   WITH DATE(),    ;
        D_TIME   WITH TIME(),    ;
        DBF_NAME WITH db_name,   ;
        MOD_REC  WITH RECNO(als),;
        TOTAL    WITH vl,        ;
        PRG_NAME WITH p_name,    ;
        PRG_LINE WITH p_line,    ;
        REM      WITH IIF(TYPE("note") = "C", note, ""), ;
        WHERE    WITH arm
SELECT (s_sav)

RETURN