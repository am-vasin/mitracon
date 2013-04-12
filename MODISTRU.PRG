*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  ModiStru      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                 ”ฎเฌจเฎข ญจฅ ไ ฉซ  แ ญฎขฎฉ แโเใชโใเฎฉ.                 บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 02.02.2000 ออผ
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
