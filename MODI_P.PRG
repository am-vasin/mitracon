*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Modi_S       Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                Программа коррекции структуры DBF-файла.                ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 11/04/1998 ══╝
PROCEDURE Modi_P

PRIVATE f_path, f_name, tag_l, tag_l_n, i, comm1
SET SAFETY OFF
SET TALK OFF
SET EXCLUSIVE OFF

***************************************************************************
*   Вот здесь: путь и имя модифицируемого файла...
***************************************************************************
f_path = "D:\TEMP\"
f_name = "PARMS"
USE (f_path+f_name) ALIAS SOURCE

*
*   Структура индексов...
*
tag_l_n = 0

DO WHILE .NOT. EMPTY(TAG(tag_l_n+1))
  tag_l_n = tag_l_n+1
  DIMENSION tag_l(tag_l_n,3)
  tag_l(tag_l_n,1) = TAG(tag_l_n)
  tag_l(tag_l_n,2) = SYS(14, tag_l_n)
  tag_l(tag_l_n,3) = SYS(2021, tag_l_n)
  ***************************************************************************
  *   Вот здесь: коррекция сеществующего индекса...
  ***************************************************************************
  *IF tag_l(tag_l_n,1) = "!!!!!!!"
  *   Если индекс надо удалить, то tag_l(tag_l_n,1) = ""  !!!!
  *  tag_l(tag_l_n,2) = ???????????????
  *  tag_l(tag_l_n,3) = ???????????????
  *ENDIF
ENDDO
***************************************************************************
*   ...а здесь: добавление индекса...
***************************************************************************
*tag_l_n = tag_l_n+1
*DIMENSION tag_l(tag_l_n,3)
*tag_l(tag_l_n,1) = "!!!!!!!!"
*tag_l(tag_l_n,2) = ?????????????????
*tag_l(tag_l_n,3) = ?????????????????
*

*
*    Структура полей
*
COPY STRUCTURE EXTENDED TO TMP_STRU
SELECT 0
USE TMP_STRU EXCLU
***************************************************************************
*   Вот здесь: модификация полей
***************************************************************************
*   Добавляем...
*APPEND BLANK
*REPLACE FIELD_NAME WITH "NEWNAME", ;
*        FIELD_TYPE WITH "C",       ;
*        FIELD_LEN  WITH 10,        ;
*        FIELD_DEC  WITH 0

*   Втавляем...
*LOCATE FOR FIELD_NAME == "NAME"
*  ... до ...
*INSERT BLANK BEFORE
*  ... или после ...
*INSERT BLANK
*REPLACE FIELD_NAME WITH "NEWNAME", ;
*        FIELD_TYPE WITH "C",       ;
*        FIELD_LEN  WITH 10,        ;
*        FIELD_DEC  WITH 0
*  ... или просто модифицируем ...
*REPLACE FIELD_NAME WITH "NEWNAME", ;
*        FIELD_TYPE WITH "C",       ;
*        FIELD_LEN  WITH 10,        ;
*        FIELD_DEC  WITH 0


*   Втавляем...
LOCATE FOR ALLTRIM(FIELD_NAME) == "SYS_ID"
*  после ...
INSERT BLANK
REPLACE FIELD_NAME WITH "VERSION", ;
        FIELD_TYPE WITH "C",       ;
        FIELD_LEN  WITH 3,         ;
        FIELD_DEC  WITH 0
INSERT BLANK
REPLACE FIELD_NAME WITH "SEND_DIR",;
        FIELD_TYPE WITH "C",       ;
        FIELD_LEN  WITH 40,        ;
        FIELD_DEC  WITH 0
INSERT BLANK
REPLACE FIELD_NAME WITH "COMM_DIR",;
        FIELD_TYPE WITH "C",       ;
        FIELD_LEN  WITH 40,        ;
        FIELD_DEC  WITH 0

BROW

USE
CREATE (f_path+"TMP_NAME") FROM TMP_STRU
USE (f_path+"TMP_NAME") EXCLUSIVE
SELECT SOURCE
*
*  Копируем и модифицируем содержимое...
*
SCAN
  *
  *  ВНИМАНИЕ! Если таблица содержит MEMO-поля,
  *  то о них следует позаботиться особо!
  DO Copy_Rec
ENDSCAN

*
*   Создаем индексы
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


*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Copy_Rec     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Копирование записи.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 11/04/1998 ──┘
PROCEDURE Copy_Rec

PRIVATE ALL
SCATTER MEMVAR
*
*  ВНИМАНИЕ! Если таблица содержит MEMO-поля,
*  то о них следует позаботиться особо!
*
*  Модифицируем или заполняем поля...
*
*m.name1 = ???
*m.name2 = ???
m.version = "001"
m.send_dir = "Z:\SENDMESS\"
m.comm_dir = "Z:\COMPEL\"
* и т. д.
SELECT TMP_NAME
APPEND BLANK
GATHER MEMVAR
SELECT SOURCE

RETURN
