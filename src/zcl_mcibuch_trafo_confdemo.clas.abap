"! <strong>Beispiel-Transformer mit Konfiguration</strong>
"! <br/>
"! Diese Klasse demonstriert die Verwendung einer Konfigurationsklasse.
"! Als Transformer ist sie komplett funktionslos.
"! <br/>
"! Diese Klasse ist eine nicht gewartete Beispiel-Implementierung zur Ergänzung
"! des Buchs "i.s.h.med MCI", ISBN 978-3-7469-1883-9. Nutzung auf eigene Gefahr.
"! Beachten Sie die Hinweise in Kapitel 9.2
"! <br/>
"! THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THE ECLIPSE PUBLIC LICENSE. ANY
"! USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM CONSTITUTES RECIPIENT'S
"! ACCEPTANCE OF THIS AGREEMENT.
CLASS zcl_mcibuch_trafo_confdemo DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_ishmed_mci_transformer.
    INTERFACES if_ishmed_mci_configurable2.

  PROTECTED SECTION.

  PRIVATE SECTION.
    "! Konfiguration des Transformers
    DATA configuration TYPE REF TO zcl_mcibuch_trafo_confdemo_cfg.

ENDCLASS.



CLASS ZCL_MCIBUCH_TRAFO_CONFDEMO IMPLEMENTATION.


  METHOD if_ishmed_mci_component~init.
    TRY.
        me->configuration = CAST zcl_mcibuch_trafo_confdemo_cfg( ir_configuration ).
      CATCH cx_sy_move_cast_error INTO DATA(cast_error).
        RAISE EXCEPTION TYPE cx_ishmed_mci_init
          EXPORTING
            previous = cast_error.
    ENDTRY.
  ENDMETHOD.


  METHOD if_ishmed_mci_configurable2~create_default_configuration.
    rr_value = NEW zcl_mcibuch_trafo_confdemo_cfg( ).
  ENDMETHOD.


  METHOD if_ishmed_mci_transformer~transform.
    DATA(institution)  = me->configuration->get_institution( ) ##needed.
    DATA(doc_org_unit) = me->configuration->get_documenting_org_unit( ) ##needed.
    " ...
  ENDMETHOD.


  METHOD if_ishmed_object~finalize ##needed.
  ENDMETHOD.

ENDCLASS.
