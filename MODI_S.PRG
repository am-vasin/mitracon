*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Modi_S       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                蹍������ ��玻オ罔� 痰珮�矣琺 DBF-�����.                �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 11/04/1998 様�
PROCEDURE Modi_S

PRIVATE f_path, f_name, tag_l, tag_l_n, i, comm1
SET SAFETY OFF
SET TALK OFF
SET EXCLUSIVE OFF

***************************************************************************
*   ��� Гメ�: �竄� � ━� ��え筥罔珮ガ��� �����...
***************************************************************************
f_path = "??????????????????????????"
f_name = "??????????????????"
USE (f_path+f_name) ALIAS SOURCE

*
*   �矗礫矣�� ┃ぅ�甌�...
*
tag_l_n = 0

DO WHILE .NOT. EMPTY(TAG(tag_l_n+1))
  tag_l_n = tag_l_n+1
  DIMENSION tag_l(tag_l_n,3)
  tag_l(tag_l_n,1) = TAG(tag_l_n)
  tag_l(tag_l_n,2) = SYS(14, tag_l_n)
  tag_l(tag_l_n,3) = SYS(2021, tag_l_n)
  ***************************************************************************
  *   ��� Гメ�: ��玻オ罔� 瓮薀痰≪鉗ィ� ┃ぅ���...
  ***************************************************************************
  *IF tag_l(tag_l_n,1) = "!!!!!!!"
  *   �甄� ┃ぅ�� ��ぎ 磴���碎, 皰 tag_l(tag_l_n,1) = ""  !!!!
  *  tag_l(tag_l_n,2) = ???????????????
  *  tag_l(tag_l_n,3) = ???????????????
  *ENDIF
ENDDO
***************************************************************************
*   ...� Гメ�: ぎ��←キ┘ ┃ぅ���...
***************************************************************************
*tag_l_n = tag_l_n+1
*DIMENSION tag_l(tag_l_n,3)
*tag_l(tag_l_n,1) = "!!!!!!!!"
*tag_l(tag_l_n,2) = ?????????????????
*tag_l(tag_l_n,3) = ?????????????????
*

*
*    �矗礫矣�� ���ォ
*
COPY STRUCTURE EXTENDED TO TMP_STRU
SELECT 0
USE TMP_STRU
***************************************************************************
*   ��� Гメ�: ��え筥��罔� ���ォ
***************************************************************************
*   ����←錺�...
*APPEND BLANK
*REPLACE FIELD_NAME WITH "NEWNAME", ;
*        FIELD_TYPE WITH "C",       ;
*        FIELD_LEN  WITH 10,        ;
*        FIELD_DEC  WITH 0

*   ���←錺�...
*LOCATE FOR ALLTRIM(FIELD_NAME) == "NAME"
*  ... ぎ ...
*INSERT BLANK BEFORE
*  ... ┼� ��甄� ...
*INSERT BLANK
*REPLACE FIELD_NAME WITH "NEWNAME", ;
*        FIELD_TYPE WITH "C",       ;
*        FIELD_LEN  WITH 10,        ;
*        FIELD_DEC  WITH 0
*  ... ┼� �牀痰� ��え筥罔珮ガ ...
*REPLACE FIELD_NAME WITH "NEWNAME", ;
*        FIELD_TYPE WITH "C",       ;
*        FIELD_LEN  WITH 10,        ;
*        FIELD_DEC  WITH 0

USE
CREATE (f_path+"TMP_NAME") FROM TMP_STRU
USE (f_path+"TMP_NAME") EXCLUSIVE
DELETE FILE TMP_STRU.DBF
SELECT SOURCE
*
*  ����珮ガ � ��え筥罔珮ガ 甌ぅ爨━��...
*
SCAN
  *
  *  ��������! �甄� ��゛��� 甌ぅ爨�� MEMO-����,
  *  皰 � ��� 甄イ礇� ����｀皋碎瘴 �甌｀!
  DO Copy_Rec
ENDSCAN

*
*   ��Г�ガ ┃ぅ�瘠
*
SELECT TMP_NAME
IF tag_l_n # 0
  FOR i = 1 TO tag_l_n
    comm1 = "INDEX ON "+tag_l(i,2)+" TAG "+tag_l(i,1)+""+ ;
    IIF(EMPTY(tag_l(i,3)), "", " FOR "+tag_l(i,3))+" ADDITIVE"
    &comm1
  ENDFOR
ENDIF
CLOSE DATA

DELETE FILE (f_path+f_name+".DBF")
DELETE FILE (f_path+f_name+".CDX")
DELETE FILE (f_path+f_name+".FPT")

RENAME (f_path+"TMP_NAME.DBF") TO (f_path+f_name+".DBF")
IF File_O (f_path+"TMP_NAME.CDX")
  RENAME (f_path+"TMP_NAME.CDX") TO (f_path+f_name+".CDX")
ENDIF
IF File_O (f_path+"TMP_NAME.FPT")
  RENAME (f_path+"TMP_NAME.FPT") TO (f_path+f_name+".FPT")
ENDIF


*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Copy_Rec     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          ����牀���┘ ����瓱.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 11/04/1998 陳�
PROCEDURE Copy_Rec

PRIVATE ALL
SCATTER MEMVAR
*
*  ��������! �甄� ��゛��� 甌ぅ爨�� MEMO-����,
*  皰 � ��� 甄イ礇� ����｀皋碎瘴 �甌｀!
*
*  ��え筥罔珮ガ ┼� ������錺� ����...
*
*m.name1 = ???
*m.name2 = ???
* � �. �.
SELECT TMP_NAME
APPEND BLANK
GATHER MEMVAR
SELECT SOURCE

RETURN
