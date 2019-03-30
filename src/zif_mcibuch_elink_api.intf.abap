"! <strong>Schnittstelle zur Klasse CL_ISHMED_TPI_ELINK_API</strong>
"! <br/>
"! Dieses Interface stellt Methoden bereit, die die Funktionen der ELINK-API
"! abbilden. Darüber kann die eigentliche Funktion zu Testzwecken simuliert werden.
"! <br/>
"! Dieses Interface ist eine nicht gewartete Beispiel-Implementierung zur Ergänzung
"! des Buchs "i.s.h.med MCI", ISBN 978-3-7469-1883-9. Nutzung auf eigene Gefahr.
"! Beachten Sie die Hinweise in Kapitel 8.3.2
"! <br/>
"! THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THE ECLIPSE PUBLIC LICENSE. ANY
"! USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM CONSTITUTES RECIPIENT'S
"! ACCEPTANCE OF THIS AGREEMENT.
INTERFACE zif_mcibuch_elink_api PUBLIC.

  INTERFACES if_ishmed_object.

  "! Siehe Klasse CL_ISHMED_TPI_ELINK_API Methode ADD.
  METHODS add
    IMPORTING
      i_object_id TYPE swo_typeid.

  "! Siehe Klasse CL_ISHMED_TPI_ELINK_API Methode DELETE.
  METHODS delete
    IMPORTING
      i_object_id TYPE swo_typeid.

  "! Siehe Klasse CL_ISHMED_TPI_ELINK_API Methode GET_EXTERNAL_KEY.
  METHODS get_external_key
    RETURNING
      VALUE(r_value) TYPE n1mci_external_key.

  "! Siehe Klasse CL_ISHMED_TPI_ELINK_API Methode GET_OBJECT_CATEGORY.
  METHODS get_object_category
    RETURNING
      VALUE(r_value) TYPE swf_clstyp.

  "! Siehe Klasse CL_ISHMED_TPI_ELINK_API Methode GET_OBJECT_ID_T.
  METHODS get_object_id_t
    RETURNING
      VALUE(rt_value) TYPE rn1tpi_object_id_t.

  "! Siehe Klasse CL_ISHMED_TPI_ELINK_API Methode GET_OBJECT_TYPE.
  METHODS get_object_type
    RETURNING
      VALUE(r_value) TYPE sibftypeid.

  "! Siehe Klasse CL_ISHMED_TPI_ELINK_API Methode GET_PROCESS_NAME.
  METHODS get_process_name
    RETURNING
      VALUE(r_value) TYPE n1mci_process_name.

  "! Siehe Klasse CL_ISHMED_TPI_ELINK_API Methode SAVE.
  METHODS save.

ENDINTERFACE.
