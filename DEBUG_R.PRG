*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Debug_R      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   蹍������ 爛��痰��罔� 矗�����罔�.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 04/03/1999 様�
PROCEDURE Debug_R
PARAMETERS als, is_add, vl, p_name, p_line, note

* als    - �����;
* is_add - �爬Л�� ぎ��←キ┘/��玻オ罔�;
* vl     - 痺���;
* p_name - ━� �牀������;
* p_line - ���ム 痰牀�� �牀������;
* note   - �爬�ョ��┘ (�ギ￥�.).

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
