"! <strong>Dateizugriffe für CSV-Datei-Import</strong>
"! <br/>
"! Dieses Interface stellt Methoden bereit, über die die Operationen zum
"! Dateizugriff von der eigentlichen Start-Konnektor-Implementierung separiert
"! werden können.
"! <br/>
"! Dieses Interface ist eine nicht gewartete Beispiel-Implementierung zur Ergänzung
"! des Buchs "i.s.h.med MCI", ISBN 978-3-7469-1883-9. Nutzung auf eigene Gefahr.
"! Beachten Sie die Hinweise in Kapitel 7.3.2
"! <br/>
"! THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THE ECLIPSE PUBLIC LICENSE. ANY
"! USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM CONSTITUTES RECIPIENT'S
"! ACCEPTANCE OF THIS AGREEMENT.
INTERFACE zif_mcibuch_file_access PUBLIC.

  "! Ausführung von OPEN DATASET i_filename FOR INPUT IN TEXT MODE ENCODING UTF-8.
  METHODS open_dataset
    IMPORTING
      i_filename TYPE string
    RAISING
      zcx_mcibuch_file_not_found
      zcx_mcibuch_mci_error.

  "! Ausführung von READ DATASET filename INTO r_result.
  METHODS read_dataset
    RETURNING
      VALUE(r_result) TYPE string
    RAISING
      zcx_mcibuch_eof_reached
      zcx_mcibuch_mci_error.

  "! Ausführung von CLOSE DATASET filename.
  METHODS close_dataset
    RAISING
      zcx_mcibuch_mci_error.

  "! Ausführung von DELETE DATASET filename.
  METHODS delete_dataset
    RAISING
      zcx_mcibuch_mci_error.

ENDINTERFACE.
