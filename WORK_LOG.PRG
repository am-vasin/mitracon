*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Work_Log     ��о�｀砒┴ ��むォ ��瓱�           13.02.98 11:22:21 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                            ����瘡 � �窶���.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Work_Log
PARAMETERS nt, u, p
*
* nt - 皀�痰 �爬�ョ����;
* u  - ��� ���讌���皀��;
* p  - ��｀腑� �メ皰.
*

PRIVATE s_sav

IF .NOT. File_O(base_path+"WORK_LOG.DBF")
  RETURN
ENDIF

s_sav = SELECT()

USE (base_path+"WORK_LOG.DBF") ORDER TAG HRON ALIAS "WL000" AGAIN
REPLACE DATE_ WITH DATE(), ;
        TIME_ WITH TIME(), ;
        NOTE  WITH nt,     ;
        WHO   WITH u,      ;
        WHERE WITH p
USE
SELECT (s_sav)

RETURN
