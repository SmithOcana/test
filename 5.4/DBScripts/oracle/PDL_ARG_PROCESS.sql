create or replace PROCEDURE           Pdl_Arg_Process(p_businessdt VARCHAR2)
AS
/**************************************************************************
Object Name  : PDL ARG Process used for inserting all eligible write off loans into STAGE table
Author      : Raghu Ganti
Created On : 04/05/2009

Change Reason :  Added code to pull the current time into transaction date.
Chnged by     : Raghu Ganti
Changed on    : 4/8/2009

Change Reason :  Restricted cso trans insertion to Texas.
Chnged by     : Raghu Ganti
Changed on    : 4/10/2009

Change Reason  : Added code to apportioning for  SLA and VPL loans.
Changed by     : Raghu Ganti
Changed on     : 4/13/2009


Change Reason  : Added code for monitoring
Changed by     : Raghu Ganti
Changed on     : 4/19/2009

Change Reason  : Added code to fix cso apportioning issue.
Changed by     : Raghu Ganti
Changed on     : 5/5/2009

Change Reason  : Commented code to update the st_lo_pmnt_plan table
Changed by     : Raghu Ganti
Changed on     : 06/12/2009

Change Reason  : Added code to insert data into teletrack table for NUS.
Changed by     : Raghu Ganti
Changed on     : 07/29/2009

Change Reason  : Added code to insert data into teletrack table for NUS based on rule.
Changed by     : Raghu Ganti
Changed on     : 07/29/2009

Change Reason  :  Added code to restrict CA State from ARG sweep.
Author         :  Raghu Ganti
Changed on     :  07/29/2009

Change Reason  : Added code to insert data into teletrack table when is_to_report_teletrack equals Y.
Changed by     : Chals
Changed on     : 07/31/2009

Change Reason  :  Added code to include CA State in ARG sweep.
Author         :  Raghu Ganti
Changed on     :  08/04/2009

Change Reason  :  Inserting into lis hitlist process table when the loan is written off
Author         :  Chals
Changed on     :  08/10/2009

Change Reason  :  Changed the logic to pull the data from ARG Process table. used sysdate instead of sysdate-1
Author         :  RKG
Changed on     :  3/25/2010

Change Reason  :  Changed the logic to pull to include credit fee and interest fee for ohio MLA loans.
Author         :  RKG
Changed on     :  3/25/2010

Change Reason  :  Modified the code to include store # 830 in write off process.
Author         :  rganti
Changed on     :  7.29.2010

Change Reason  :  Added code to run the sweep just for the stores provided by business.
Author         :  rganti
Changed on     :  12/10/2010

Change Reason  :  Modified code to include all store for this process.
Author         :  rganti
Changed on     :  3/24/2011


**************************************************************************/
   v_loancode                NUMBER;
   v_loantrancode            NUMBER;
   v_balance                 NUMBER (10, 2);
   v_store                   NUMBER;
   v_totaldue                NUMBER (10, 2);
   v_procedurename           VARCHAR2 (100);
   v_bocode                  NUMBER;
   v_checkstatusid           VARCHAR2 (100);
   v_appliedtoadvprincipal   NUMBER (10, 2);
   v_appliedtoadvfee         NUMBER (10, 2);
   v_appliedtonsffee         NUMBER (10, 2) := 0;
   v_appliedtolatefee        NUMBER (10, 2) := 0;
   v_appliedtowoadvprin      NUMBER (10, 2);
   v_appliedtowoadvfee       NUMBER (10, 2);
   v_appliedtowofee          NUMBER (10, 2);
   v_appliedtocreditfee      NUMBER (10, 2) := 0;
   v_appliedtointerestfee    NUMBER (10, 2) := 0;
   v_appliedtomlacreditfee   NUMBER (10,2)   :=0 ;
   v_nfcount                 NUMBER;
   v_lfcount                 NUMBER;
   v_ancount                 NUMBER;
   v_fncount                 NUMBER;
   v_infncount               NUMBER;
   v_vfncount                NUMBER;
   v_mlacrfncount            NUMBER;
   v_nf                      VARCHAR2 (10);
   v_lf                      VARCHAR2 (10);
   v_an                      VARCHAR2 (10);
   v_fn                      VARCHAR2 (10);
   v_wo                      VARCHAR2 (10);
   v_wofee                   VARCHAR2 (10);
   v_infn                    VARCHAR2 (10);
   v_vfn                     VARCHAR2 (10);
   v_crfn                    VARCHAR2(10);
   v_csofee                  NUMBER (10, 2);
   v_csocount                NUMBER;
   v_appliedtocso            NUMBER (10, 2) := 0;
   v_cso                     VARCHAR2 (10);
   v_csotrancode             NUMBER;
   v_state                   VARCHAR2 (10);
   v_isepp                   CHAR (1);
   v_loantype                VARCHAR2 (10);
   c_pdlwriteoffloans        sys_refcursor;
   v_SEQ_AUTO_EMAIL_JOB NUMBER:=-1;
   --v_seqteletrackprocess number;
   v_ttisNewCustAllowedValue NUMBER :=0;
   v_rulevaluecount NUMBER:=0;

   v_is_to_report_teletrack    CHAR(1);
   v_new_teletrack_nbr ST_LO_MASTER.NEW_TELETRACK_NBR%TYPE;

   v_last_name    BO_MASTER.LAST_NAME%TYPE;
   v_first_name BO_MASTER.FIRST_NAME%TYPE;
   v_ssn    BO_MASTER.SSN%TYPE;


   CURSOR c1
   IS
       SELECT *
        FROM ca_ss_store;
       -- where st_code in (select a.st_code from arg_sweep_stores a);

BEGIN
    SELECT SEQ_AUTO_EMAIL_JOB.NEXTVAL INTO v_SEQ_AUTO_EMAIL_JOB FROM dual;


   FOR c1_rec IN c1
   LOOP

            SELECT COUNT(*)
                      INTO v_rulevaluecount
                      FROM ca_br_set_map cbsm,
                           ca_br_rule_value cbrv,
                           ca_br_set_store_map cbrssm
                     --   LT_WEEKEND_TYPE lwt
           WHERE           cbsm.br_code = cbrv.br_code
                       --  AND lwt.br_value = cbrv.VALUE
                       AND cbsm.br_set_code = cbrssm.br_set_code
                       AND cbrssm.st_code = c1_rec.st_code
                       AND cbsm.br_id = 'ttisNewCustAllowed'
                       AND TRUNC (TO_DATE (p_businessdt, 'mm/dd/yyyy')) >=
                                                               TRUNC (cbrv.start_date)
                       AND TRUNC (TO_DATE (p_businessdt, 'mm/dd/yyyy')) <=
                              NVL (TRUNC (cbrv.end_date),
                                   TO_DATE (p_businessdt, 'MM/DD/YYYY')
                                  );
           IF v_rulevaluecount > 0 THEN

             SELECT DISTINCT cbrv.VALUE
                      INTO v_ttisNewCustAllowedValue
                      FROM ca_br_set_map cbsm,
                           ca_br_rule_value cbrv,
                           ca_br_set_store_map cbrssm
                     --   LT_WEEKEND_TYPE lwt
           WHERE           cbsm.br_code = cbrv.br_code
                       --  AND lwt.br_value = cbrv.VALUE
                       AND cbsm.br_set_code = cbrssm.br_set_code
                       AND cbrssm.st_code = c1_rec.st_code
                       AND cbsm.br_id = 'ttisNewCustAllowed'
                       AND TRUNC (TO_DATE (p_businessdt, 'mm/dd/yyyy')) >=
                                                               TRUNC (cbrv.start_date)
                       AND TRUNC (TO_DATE (p_businessdt, 'mm/dd/yyyy')) <=
                              NVL (TRUNC (cbrv.end_date),
                                   TO_DATE (p_businessdt, 'MM/DD/YYYY')
                                  );
                                  END IF;
      OPEN c_pdlwriteoffloans FOR
         --Select A.Loan_Code, A.St_Code, A.total_due, B.Total_Due,a.Bo_Code ,
         SELECT   slm.loan_code, slm.st_code, a.woamt, slm.total_due,
                  slm.bo_code, slm.check_status_id, NVL (cso_fee, 0) csofee,
                  css.state_id, slm.is_epp, slm.loan_type,
                  slm.is_to_report_teletrack, slm.new_teletrack_nbr,
          bm.last_name, bm.first_name, bm.ssn
             FROM arg_process a, st_lo_master slm, ca_ss_store css, bo_master bm
            WHERE TRUNC (a.date_created) =trunc(sysdate)
              AND a.loannbr = slm.loan_code
              AND slm.st_code = css.st_code
          AND bm.bo_code = slm.bo_code
              AND slm.loan_status_id = 'OPN'
              AND a.process_flag IN( 'NOP','PRO')
              AND slm.st_code = c1_rec.st_code
              and slm.total_due > slm.rtn_Fee_amt+slm.late_fee_amt
         ORDER BY slm.loan_code;

      --and slm.loan_code =15354946  ;

      --and total_due = 0
      LOOP
         FETCH c_pdlwriteoffloans
          INTO v_loancode, v_store, v_balance, v_totaldue, v_bocode,
               v_checkstatusid, v_csofee, v_state, v_isepp, v_loantype,
               v_is_to_report_teletrack, v_new_teletrack_nbr,
           v_last_name, v_first_name, v_ssn;

         v_appliedtoadvprincipal := 0;
         v_appliedtoadvfee := 0;
         v_appliedtonsffee := 0;
         v_appliedtolatefee := 0;
         v_appliedtowoadvprin := 0;
         v_appliedtowoadvfee := 0;
         v_appliedtowofee := 0;
         v_appliedtocreditfee := 0;
         v_appliedtointerestfee := 0;
         v_nfcount := 0;
         v_lfcount := 0;
         v_ancount := 0;
         v_fncount := 0;
       --  v_csofee := 0;
         v_csocount := 0;
         v_appliedtocso := 0;
         v_infncount := 0;
         v_vfncount := 0;
         EXIT WHEN c_pdlwriteoffloans%NOTFOUND;

         /* Updating ST_LO_Master table Loan_Status_Id , WO_Date ,Date_Updated,Update_By Fields*/
         --IF v_checkstatusid <> 'PPN'
         IF v_checkstatusid <> 'PPN'
         THEN
            UPDATE st_lo_master
               SET loan_status_id = 'WO',
                   wo_date = TO_DATE (p_businessdt, 'MM/DD/YYYY'),
                   date_updated = SYSDATE,
                   check_status_id = 'WO',
                   updated_by = 111111
             WHERE loan_code = v_loancode;
         ELSIF v_checkstatusid = 'PPN'
         THEN
            UPDATE st_lo_master
               SET loan_status_id = 'WO',
                   wo_date = TO_DATE (p_businessdt, 'MM/DD/YYYY'),
                   ppn_check_status_id ='WO',
				   check_status_id = 'WO',
                   date_updated = SYSDATE,
                   updated_by = 111111
             WHERE loan_code = v_loancode;
         END IF;

         IF v_isepp = 'N'
         THEN
            /* Updating BO_CHECK_REGISTER table check_status_id ,wo_date,date_updated,updated_by fields */
            UPDATE bo_check_register
               SET check_status_id = 'WO',
                   date_updated = TO_DATE (p_businessdt, 'MM/DD/YYYY'),
                   updated_by = 111111
             WHERE loan_code = v_loancode;
        /* ELSIF (v_isepp = 'Y' AND v_state <> 'MI')
         THEN
            UPDATE st_lo_pmnt_plan
               SET check_status = 'WO',
                   date_updated = SYSDATE
             WHERE loan_code = v_loancode
               AND check_num IS NOT NULL
               AND pay_flag = 'N';*/
         ELSIF (v_isepp = 'Y' AND v_state = 'MI')
         THEN
            UPDATE bo_check_register
               SET check_status_id = 'WO',
                   date_updated = TO_DATE (p_businessdt, 'MM/DD/YYYY'),
                   updated_by = 111111
             WHERE loan_code = v_loancode;
         END IF;

         SELECT seq_stlotrans.NEXTVAL
           INTO v_loantrancode
           FROM DUAL;

         SELECT seq_csotrans.NEXTVAL
           INTO v_csotrancode
           FROM DUAL;

         DBMS_OUTPUT.PUT_LINE
            ('/*****************************************************************/'
            );
         DBMS_OUTPUT.PUT_LINE ('BO Code        = ' || v_bocode);
         DBMS_OUTPUT.PUT_LINE ('Loan Code       = ' || v_loancode);
         DBMS_OUTPUT.PUT_LINE ('Store              = ' || v_store);
         DBMS_OUTPUT.PUT_LINE ('Loan TranCode = ' || v_loantrancode);

         /* Inserting a record into ST_LO_Trans with Tran_Id = 'WO' */
         INSERT INTO st_lo_trans
                     (loan_tran_code, st_code, loan_code, tran_id,
                      tran_date,
                      total_due, change_amt, void_id, other_st_code,
                      orig_tran_code, loan_status_id, check_status_id,
                      bo_status_id, wo_amt, date_created, created_by, flag_dw
                     )
              VALUES (v_loantrancode, v_store, v_loancode, 'WO',
                      TO_DATE (   TO_CHAR (TO_DATE (p_businessdt,
                                                    'MM/DD/YYYY'),
                                           'MM/DD/YYYY'
                                          )
                               || ' '
                               || TO_CHAR (SYSDATE, 'HH:MI:ss AM'),
                               'mm/dd/yyyy hh:mi:ss AM'
                              ),
                      v_totaldue, 0, 'N', NULL,
                      NULL, 'WO', 'WO',
                      'WO', v_balance, SYSDATE, 111111, 'N'
                     );

         IF v_state = 'TX'
         THEN
            INSERT INTO cso_trans
                        (loan_code, cso_tran_code, loan_tran_code, inst_num,
                         tran_id,
                         tran_date,
                         cso_tran_amt, cso_total_due, cso_check_status,
                         cso_status, orig_tran_id, void_flg, date_created,
                         created_by, flag_dw, date_dw, cso_earned_fee,
                         rtn_tran_id, rtn_clr_tran_code
                        )
                 VALUES (v_loancode, v_csotrancode, v_loantrancode, NULL,
                         'WO',
                         TO_DATE (   TO_CHAR (TO_DATE (p_businessdt,
                                                       'MM/DD/YYYY'
                                                      ),
                                              'MM/DD/YYYY'
                                             )
                                  || ' '
                                  || TO_CHAR (SYSDATE, 'HH:MI:ss AM'),
                                  'mm/dd/yyyy hh:mi:ss AM'
                                 ),
                         0, 0, 'WO',
                         'WO', NULL, 'N', SYSDATE,
                         111111, 'N', NULL, 0,
                         NULL, NULL
                        );
         END IF;

          /* Inserting into ST_LO_Apportions WITH APPORTION_ID = 'N'*/
         /* INSERT INTO st_lo_apportions
                      (loan_tran_code, apportion_id, apportion_amt, date_created,
                       created_by
                      )
               VALUES (v_loantrancode, 'N', -v_balance, SYSDATE,
                       111111
                      );

          /* Inserting into ST_LO_Apportions WITH APPORTION_ID = 'WO'*/
         /* INSERT INTO st_lo_apportions
                      (loan_tran_code, apportion_id, apportion_amt, date_created,
                       created_by
                      )
               VALUES (v_loantrancode, 'WO', v_balance, SYSDATE,
                       111111
                      );*/
         IF v_checkstatusid <> 'HLD'
         THEN
            SELECT COUNT (*)
              INTO v_nfcount
              FROM (SELECT   COUNT (slt.loan_tran_code)
--             INTO v_nfcount
                    FROM     st_lo_trans slt, st_lo_apportions sla
                       WHERE slt.loan_tran_code = sla.loan_tran_code
                         AND apportion_id = 'NF'
                         AND slt.loan_code = v_loancode
                    GROUP BY apportion_id);

            SELECT COUNT (*)
              INTO v_lfcount
              FROM (SELECT   COUNT (slt.loan_tran_code)
--             INTO v_nfcount
                    FROM     st_lo_trans slt, st_lo_apportions sla
                       WHERE slt.loan_tran_code = sla.loan_tran_code
                         AND apportion_id = 'LF'
                         AND slt.loan_code = v_loancode
                    GROUP BY apportion_id);

            IF v_loantype IN ('SLA', 'VPL','MLA')
            THEN
               -- FOR SLA INFN
               SELECT COUNT (*)
                 INTO v_infncount
                 FROM (SELECT   COUNT (slt.loan_tran_code)
--             INTO v_nfcount
                       FROM     st_lo_trans slt, st_lo_apportions sla
                          WHERE slt.loan_tran_code = sla.loan_tran_code
                            AND apportion_id = 'INFN'
                            AND slt.loan_code = v_loancode
                       GROUP BY apportion_id);

               SELECT COUNT (*)
                 INTO v_vfncount
                 FROM (SELECT   COUNT (slt.loan_tran_code)
--             INTO v_nfcount
                       FROM     st_lo_trans slt, st_lo_apportions sla
                          WHERE slt.loan_tran_code = sla.loan_tran_code
                            AND apportion_id = 'VFN'
                            AND slt.loan_code = v_loancode
                       GROUP BY apportion_id);

                       --for mla
                                      SELECT COUNT (*)
                 INTO v_mlacrfncount
                 FROM (SELECT   COUNT (slt.loan_tran_code)
--             INTO v_nfcount
                       FROM     st_lo_trans slt, st_lo_apportions sla
                          WHERE slt.loan_tran_code = sla.loan_tran_code
                            AND apportion_id = 'CRFN'
                            AND slt.loan_code = v_loancode
                       GROUP BY apportion_id);

            END IF;

            DBMS_OUTPUT.PUT_LINE ('late fee count' || v_lfcount);

            IF v_nfcount > 0
            THEN
               SELECT   apportion_id, SUM (apportion_amt)
                   INTO v_nf, v_appliedtonsffee
                   FROM st_lo_trans slt, st_lo_apportions sla
                  WHERE slt.loan_tran_code = sla.loan_tran_code
                    AND apportion_id = 'NF'
                    AND slt.loan_code = v_loancode
               GROUP BY apportion_id;
            END IF;

            IF v_lfcount > 0
            THEN
               SELECT   apportion_id, SUM (apportion_amt)
                   INTO v_lf, v_appliedtolatefee
                   FROM st_lo_trans slt, st_lo_apportions sla
                  WHERE slt.loan_tran_code = sla.loan_tran_code
                    AND apportion_id = 'LF'
                    AND slt.loan_code = v_loancode
               GROUP BY apportion_id;
            END IF;

            IF v_loantype IN ('SLA', 'VPL','MLA')
            THEN
               IF v_infncount > 0
               THEN
                  SELECT   apportion_id, SUM (apportion_amt)
                      INTO v_infn, v_appliedtocreditfee
                      FROM st_lo_trans slt, st_lo_apportions sla
                     WHERE slt.loan_tran_code = sla.loan_tran_code
                       AND apportion_id IN ('INF')
                       AND slt.loan_code = v_loancode
                  GROUP BY apportion_id;
               END IF;

               IF v_vfncount > 0
               THEN
                  SELECT   apportion_id, SUM (apportion_amt)
                      INTO v_vfn, v_appliedtointerestfee
                      FROM st_lo_trans slt, st_lo_apportions sla
                     WHERE slt.loan_tran_code = sla.loan_tran_code
                       AND apportion_id IN ('VFN')
                       AND slt.loan_code = v_loancode
                  GROUP BY apportion_id;
               END IF;
            END IF;

            DBMS_OUTPUT.PUT_LINE ('nsf fee' || v_appliedtonsffee);
            DBMS_OUTPUT.PUT_LINE ('late fee' || v_appliedtolatefee);

            IF v_loantype IN ('SLA', 'VPL','MLA')
            THEN
               IF v_infncount > 0
               THEN
                  SELECT 'INFN', SUM (apportion_amt)
                    INTO v_infn, v_appliedtocreditfee
                    FROM st_lo_trans slt, st_lo_apportions sla
                   WHERE slt.loan_tran_code = sla.loan_tran_code
                     AND apportion_id IN ('INF', 'INFN')
                     AND slt.loan_code = v_loancode;
               END IF;

               IF v_loantype = 'VPL'
               THEN
                  IF v_vfncount > 0
                  THEN
                     SELECT 'VFN', SUM (apportion_amt)
                       INTO v_vfn, v_appliedtointerestfee
                       FROM st_lo_trans slt, st_lo_apportions sla
                      WHERE slt.loan_tran_code = sla.loan_tran_code
                        AND apportion_id IN ('VFN', 'VF')
                        AND slt.loan_code = v_loancode;
                  END IF;
               END IF;

               --MLA
                IF v_infncount > 0
               THEN
                  SELECT 'CRFN', SUM (apportion_amt)
                    INTO v_crfn, v_appliedtomlacreditfee
                    FROM st_lo_trans slt, st_lo_apportions sla
                   WHERE slt.loan_tran_code = sla.loan_tran_code
                     AND apportion_id IN ('CRF', 'CRFN')
                     AND slt.loan_code = v_loancode;
               END IF;
            END IF;
         END IF;

         IF v_csofee > 0
         THEN
            SELECT 'CBF', SUM (apportion_amt)
              INTO v_cso, v_appliedtocso
              FROM st_lo_trans slt, cso_distribution sla
             WHERE slt.loan_tran_code = sla.loan_tran_code
               --AND apportion_id IN ('CBF','CGF',)
               AND slt.loan_code = v_loancode;
         END IF;

         DBMS_OUTPUT.PUT_LINE ('APPLIED TO CSO FEE : ' || v_appliedtocso);

         SELECT COUNT (slt.loan_tran_code)
           INTO v_ancount
           FROM st_lo_trans slt, st_lo_apportions sla
          WHERE slt.loan_tran_code = sla.loan_tran_code
            AND apportion_id IN ('A', 'AN')
            AND slt.loan_code = v_loancode;

         IF v_ancount > 0
         THEN
            SELECT 'AN', SUM (apportion_amt)
              INTO v_an, v_appliedtoadvprincipal
              FROM st_lo_trans slt, st_lo_apportions sla
             WHERE slt.loan_tran_code = sla.loan_tran_code
               AND apportion_id IN ('A', 'AN')
               AND slt.loan_code = v_loancode;
         END IF;

         SELECT COUNT (slt.loan_tran_code)
           INTO v_fncount
           FROM st_lo_trans slt, st_lo_apportions sla
          WHERE slt.loan_tran_code = sla.loan_tran_code
            AND apportion_id IN ('F', 'FN')
            AND slt.loan_code = v_loancode;

         IF v_ancount > 0
         THEN
            SELECT 'FN', SUM (apportion_amt)
              INTO v_an, v_appliedtoadvfee
              FROM st_lo_trans slt, st_lo_apportions sla
             WHERE slt.loan_tran_code = sla.loan_tran_code
               AND apportion_id IN ('F', 'FN')
               AND slt.loan_code = v_loancode;
         END IF;

         /*Inserting into ST_LO_Apportions WITH APPORTION_ID = 'N'*/
         INSERT INTO st_lo_apportions
                     (loan_tran_code, apportion_id, apportion_amt,
                      date_created, created_by
                     )
              VALUES (v_loantrancode, 'AN', -v_appliedtoadvprincipal,
                      SYSDATE, 111111
                     );

         INSERT INTO st_lo_apportions
                     (loan_tran_code, apportion_id, apportion_amt,
                      date_created, created_by
                     )
              VALUES (v_loantrancode, 'FN', -v_appliedtoadvfee,
                      SYSDATE, 111111
                     );

         INSERT INTO st_lo_apportions
                     (loan_tran_code, apportion_id,
                      apportion_amt, date_created,
                      created_by
                     )
              VALUES (v_loantrancode, 'NF',
                      - (v_appliedtonsffee + v_appliedtolatefee), SYSDATE,
                      111111
                     );

         IF v_csofee > 0
         THEN
            IF (v_appliedtocso > 0)
            THEN
               INSERT INTO cso_distribution
                           (loan_tran_code, cso_tran_code, apportion_id,
                            apportion_amt, date_created, created_by
                           )
                    VALUES (v_loantrancode, v_csotrancode, 'CBF',
                            -v_appliedtocso, SYSDATE, 111111
                           );
            END IF;
         END IF;

         IF v_loantype IN ('SLA', 'VPL','MLA')
         THEN
            INSERT INTO st_lo_apportions
                        (loan_tran_code, apportion_id, apportion_amt,
                         date_created, created_by
                        )
                 VALUES (v_loantrancode, 'INFN', - (v_appliedtocreditfee),
                         SYSDATE, 111111
                        );
            INSERT INTO st_lo_apportions
                        (loan_tran_code, apportion_id, apportion_amt,
                         date_created, created_by
                        )
                 VALUES (v_loantrancode, 'CRFN', - (v_appliedtomlacreditfee),
                         SYSDATE, 111111
                        );



            IF v_loantype = 'VPL'
            THEN
               INSERT INTO st_lo_apportions
                           (loan_tran_code, apportion_id,
                            apportion_amt, date_created, created_by
                           )
                    VALUES (v_loantrancode, 'VFN',
                            - (v_appliedtointerestfee), SYSDATE, 111111
                           );
            END IF;
         END IF;

         /* Inserting into ST_LO_Apportions WITH APPORTION_ID = 'WO'*/
         INSERT INTO st_lo_apportions
                     (loan_tran_code, apportion_id,
                      apportion_amt,
                      date_created, created_by
                     )
              VALUES (v_loantrancode, 'WO',
                        v_appliedtoadvfee
                      + v_appliedtoadvprincipal
                      + ABS (v_appliedtocso)
                      + ABS (NVL (v_appliedtocreditfee, 0))
                      + ABS (NVL (v_appliedtointerestfee, 0))
                      + ABS( NVL(v_appliedtomlacreditfee,0)),
                      SYSDATE, 111111
                     );

         INSERT INTO st_lo_apportions
                     (loan_tran_code, apportion_id,
                      apportion_amt, date_created, created_by
                     )
              VALUES (v_loantrancode, 'WOFEE',
                      v_appliedtonsffee + v_appliedtolatefee, SYSDATE, 111111
                     );

         /* Updating BO_Status_Id in BO_Master table to WO*/
         UPDATE bo_master
            SET bo_status_id = 'WO',
                date_updated = TO_DATE (p_businessdt, 'MM/DD/YYYY'),
                updated_by = 111111
          WHERE bo_code = v_bocode;

           /*  Code for inserting data into Teletrack table for NUS */
           IF v_rulevaluecount > 0 THEN
           IF v_ttisNewCustAllowedValue =1 THEN
           IF v_is_to_report_teletrack = 'Y' THEN
      INSERT INTO TELETRACK_TO_PROCESS
            (
                  LOAN_CODE,  TRAN_CODE,
                  BO_CODE,ST_CODE,
                  ENGINE_TYPE,PRODUCT_TYPE,
                  IS_PROCESSED,     TRAN_ID,
                  STATUS,TRAN_STATUS,
                  DATE_CREATED, TELETRACK_CODE, SEQ_NUM
            )
            VALUES
            (
                 v_loancode,v_loantrancode,
                 v_bocode,  v_store,
                 'TIP', 'PDL','NOP', 'WO',
                 0,  NULL, SYSDATE, v_new_teletrack_nbr,
                 SEQ_TELETRACKTOPROCESS.NEXTVAL
      );
      END IF;
      END IF;
      END IF;

    -- Inserting into lis hitlist process table when the loan is written off
    INSERT INTO LIS_HITLIST_PROCESS
    (
        SEQ_NUM, ST_CODE, BO_CODE, LAST_NAME, FIRST_NAME, SSN, REASON_ID,
        COMMENTS, STATUS, CREATE_BY, DATE_CREATED, UPDATED_BY, DATE_UPDATED,HITLIST_DATE
    )
    SELECT SEQ_LISHITLISTPROCESS.NEXTVAL, v_store, v_bocode, v_last_name, v_first_name, v_ssn, 7,
    NULL, 'NOP', 1, SYSDATE, 1, SYSDATE, to_date(p_businessdt,'mm/dd/yyyy') FROM DUAL;

      END LOOP;

      COMMIT;

      CLOSE c_pdlwriteoffloans;
   END LOOP;

   INSERT INTO AUTO_EMAIL_JOB(SEQ_NUM,SCHEDULE_ID,SCHEDULE_TIME,JOB_DESCRIPTION,STATUS,TOTAL_AMOUNT,CREATED_BY,DATE_CREATED,
              UPDATED_BY,DATE_UPDATED,PROCESS_TYPE,TOTAL_AMOUNT_NOP,COMMENTS,PROCESS_DESCRIPTION)
            VALUES(v_SEQ_AUTO_EMAIL_JOB,38,'22:30','Upload  PDL ARG Sweep Transactions into Staging Tables','Failure',NULL,1,SYSDATE,
              NULL,NULL,NULL,NULL,'Process started','Upload PDL ARG Sweep Transactions into Staging Tables');
COMMIT;


UPDATE AUTO_EMAIL_JOB SET STATUS ='Success' , UPDATED_BY=1,DATE_UPDATED=SYSDATE, COMMENTS='Successfully completed' WHERE SEQ_NUM=v_SEQ_AUTO_EMAIL_JOB;

   COMMIT;


END Pdl_Arg_Process; 
