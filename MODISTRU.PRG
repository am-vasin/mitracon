*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� ModiStru     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                 ��爼�牀���┘ ����� � ��〓� 痰珮�矣牀�.                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 02.02.2000 様�
PROCEDURE ModiStru
PARAMETERS pth, old_file, new_file, sav_file
PRIVATE comm_s

SET DELETED ON
SET EXCLUSIVE OFF

USE (pth+new_file)
APPEND FROM (pth+old_file)
USE
comm_s = "! REN "+pth+old_file+".* "+sav_file+".*"
&comm_s
comm_s = "! REN "+pth+new_file+".* "+old_file+".*"
&comm_s

RETURN
