"! <strong>Beispiel-Implementierung Ereignis-Filter</strong>
"! <br/>
"! Dieses Beispiel zur BAdI-Definition ISHMED_MCI_EVMG_RUN zeigt, wie Ereignisse
"! vor Start von MCI-Prozessen gefiltert werden können.
"! <br/>
"! Diese Klasse ist eine nicht gewartete Beispiel-Implementierung zur Ergänzung
"! des Buchs "i.s.h.med MCI", ISBN 978-3-7469-1883-9. Nutzung auf eigene Gefahr.
"! Beachten Sie die Hinweise in Kapitel 10.1.1
"! <br/>
"! THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THE ECLIPSE PUBLIC LICENSE. ANY
"! USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM CONSTITUTES RECIPIENT'S
"! ACCEPTANCE OF THIS AGREEMENT.
CLASS zcl_mcibuch_badi_demo_filter DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_ex_ishmed_mci_evhndl_run.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_MCIBUCH_BADI_DEMO_FILTER IMPLEMENTATION.


  METHOD if_ex_ishmed_mci_evhndl_run~adjust.

    " sicherstellen, dass es sich um ein Dokumenten-Ereignis handelt
    " (sollte bereits durch die Filterkriterien abgedeckt sein, daher hier
    " relativ "harte" Prüfung")
    DATA(application_id) = ir_event->get_evapplid( ).
    ASSERT application_id = 'DOCUMENT'.

    " Schlüssel des Dokuments ermitteln
    DATA(document_key) = CONV rn2doc_key( ir_event->get_business_key( ) ).

    " Dokumentverwaltungsdaten lesen
    CALL FUNCTION 'ISH_N2_MEDDOC_CLEAR_BUFFER'
      EXPORTING
        im_document_key = document_key.
    DATA document_data TYPE rn2docdata.
    CALL FUNCTION 'ISH_N2_MEDDOC_GETADMINDATA'
      EXPORTING
        im_document_key = document_key
      IMPORTING
        ex_docdata      = document_data.

    " Prozess nicht auslösen, wenn Dokument mit dem Kennzeichen "intern"
    " versehen ist
    IF document_data-pcode = 'INTL'.
      c_run_process = abap_false.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
