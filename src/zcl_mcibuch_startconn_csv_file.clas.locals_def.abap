"! Lokale Klasse zur Kapselung der Dateizugriffe
CLASS lcl_file_access DEFINITION FINAL.

  PUBLIC SECTION.

    INTERFACES zif_mcibuch_file_access.

  PRIVATE SECTION.
    DATA filename TYPE string.

ENDCLASS.
