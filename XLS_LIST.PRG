*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� XLS_List     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                         ���甌� � Excel 筮爼�皀                         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 08.08.2008 様�
PROCEDURE XLS_List
PARAMETERS l_num

PRIVATE tmpList, tmpName, tmpSPath, nf, s_sav, n_sec, mss

m.tmpList = IIF(TYPE("m.l_num")="C", VAL(m.l_num), m.l_num)
m.tmpName = ""

m.tmpSPath = ""
IF File_O(m.prmo_path+"XLS_List.txt")
  m.nf = FOPEN(m.prmo_path+"XLS_List.txt")
  m.tmpSPath = FGETS(m.nf)
  = FCLOSE(m.nf)
ENDIF

m.tmpName = New_File(m.tmpSPath, "xls")

IF EMPTY(m.tmpName)
  RETURN
ENDIF

m.nf = RAT("\", m.tmpName)
m.tmpSPath = LEFT(m.tmpName, m.nf)
DELETE FILE (m.prmo_path+"XLS_List.txt")
m.nf = FCREATE(m.prmo_path+"XLS_List.txt")
= FPUTS(m.nf, m.tmpSPath)
= FCLOSE(m.nf)

m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"WIN_TASK") SHARED AGAIN ORDER TAG ARM
IF BOF() .OR. EOF() .OR. .NOT. EMPTY(ARM)
  APPEND BLANK
ENDIF
REPLACE	ARM			WITH m.arm,			;
		TASK_DT		WITH DATE(),		;
		TASK_TM		WITH SECONDS(),		;
		TASK_TYPE	WITH "XLS_LIST",	;
		TASK_PRM	WITH ALLTRIM(STR(m.tmpList))+CHR(13)+m.tmpName
DO Wt_Mess WITH "��爼�牀���┘ �����"

m.nf = 0
m.n_sec = 10
DO WHILE m.nf < m.n_sec
  m.nf = m.nf+1
  WAIT WINDOW "" TIMEOUT 1
  GO RECNO()
  IF EMPTY(TASK_TYPE)
    IF ARM # m.arm
      EXIT
    ENDIF
    m.n_sec = 200
  ENDIF
ENDDO

DO Wt_Mess

IF ARM # m.arm
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"���� 痲�爼�牀���"
  mss[3] = ""
  DO Out_Mess WITH 5, "mss"
ELSE
  REPLACE	ARM			WITH "",		;
			TASK_DT		WITH CTOD(""),	;
			TASK_TM		WITH 0,			;
			TASK_TYPE	WITH "",		;
			TASK_PRM	WITH ""
  IF m.n_sec = 200
    DIMENSION mss[3]
    mss[1] = ""
    mss[2] = CHR(0)+"���� �� 痲�爼�牀���!"
    mss[3] = ""
    DO Out_Mess WITH 7, "mss"
  ELSE
    DIMENSION mss[3]
    mss[1] = ""
    mss[2] = CHR(0)+"���竅皋皀 瓮燿ム �ョ�皋 � Win_Sale � ��≒�爬皀 ���謔��..."
    mss[3] = ""
    DO Out_Mess WITH 7, "mss"
  ENDIF
ENDIF
USE

SELECT (m.s_sav)

RETURN
