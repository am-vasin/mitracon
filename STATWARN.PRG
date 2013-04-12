function statwarn
PARAMETERS pref, name, prod
PRIVATE result
result=.F.
if File_O(base_path+"STATICS.DBF")
  SELECT 0
  USE base_path+"STATICS.DBF"
  LOCATE FOR ((STATICS.FIELD = "NAME") AND (name = TRIM(SUBS))) OR;
             ((STATICS.FIELD = "PRODUCER") AND (TRIM(prod) == TRIM(SUBS))) OR;
             ((STATICS.FIELD = "PREFIX") AND (TRIM(SUBS) == LTRIM(pref)))
  result=FOUND()
  USE
endif
RETURN result
