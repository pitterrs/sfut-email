# Supplier Follow Up Tool E-mail

This package was created in order to attend the requirements related to the **Supplier Follow Up Tool E-mail** solution.
Here, you will find the development approach taken and the technical artifacts related to the excel creation and the e-mail sending/receiving procedure.

> **Project Details**
> 
>  - **Package:** ZMM_SFUT_EMAIL
>  - **Source System:** SAP AG
>  - **Product Owner:** Rafael Cruz (CruzRafaelS@JohnDeere.com)

## Change Management

The following Services Now changes and SAP transport requests were created in order to maintain or implement functionalities directly related to the IMMEX Prototype Automation project.

|Service Now Change | SAP Request |Request Description                    |
|-------------------|-------------|---------------------------------------|
|CHG0312114         |`CAGK9C1ZWN` |Supplier Follow Up Tool - E-mail Functionality       |

## Technical Approach

This solution is composed of three main processes that are directly related to the **Supplier Follow Up** project, started through the already created report transactions `/DEERE/ZMDELINQL` (Common Delinquent **live** Trasaction) and `/DEERE/ZMDELINQH` (Common Delinquent **History** Report).

The main idea is to enable the **history report** to send e-mails with the transaction data to pre-configured suppliers in order to accomplish the Follow Up process.

The e-mails to be sent will have the report output data attached into an excel file customized for this follow up procedure.

The three mentioned processes will be tackled on this document to give a technical overview related to the Supplier Follow Up e-mail solution.

> **Note:**
> 
> The Supplier Follow Up E-mail solution is a completely "Plug-and-play" procedure, meaning that you can use the e-mail process tackled on this documentation to send the follow up data through any other solutions that might have the information expected on the attachment file. 

The structure `ZMAL_SFUT_SUPPLIER_DATA` will be the one responsible for the gathering and retrieving process data during the sending and receiving procedure. This structure will also define the excel attachment layout.

## Configuration Procedure

All the configuration necessary for this solution will be set through the transaction `ZM_SFUT_EMAIL`.

This transaction calls the view cluster `ZMA0VC_SFUT_EMAIL`, created through the *SE54 transaction* that will be handling the following transparent tables:

* `ZMA0_SFUT_EMAIL`
Stores the subject text and the access to the body content object per purchasing organization to be inserted on the e-mail attachment.

* `ZMA0_SFUT_RECV`
Set the e-mail address for the attachment receivers per purchasing organization, supplier code and plant information.

* `ZMA0_SFUT_SEND`
Set the sender for the e-mail ignoring the logic that will get the sender based on the MRP planner or buyer custom master data register.

* `ZMA0_SFUT_REAS`
Store the reasons to be available for selection on the e-mail attachment. 

### Standard Sender Logic

If the attachment has any register with the **material** and **MRP planner** properly filled, the logic will consider sending the follow up data with the e-mail address from the table `ZCSVT024D` *(User Defined Table for MRP Controller)*. The selection will be executed based on the excel file columns **WERKS** (Plant) and **DISPO** (MRP Planner). Otherwise, when the registers that compose the excel file doesn't have material or MRP planner data, the e-mail address from the table `ZCSVT024` *(User Defined Table for Purchasing Group)* will be considered for the e-mail sender definition.

> **Note**:
> If the e-mail attachment contains the two mentioned scenarios, the sender e-mails will be all considered and the selected planners/buyers will receive the follow up data in a e-mail copy.

As there is no possibility to define more than one sender per e-mail, the first selected address based on the standard and custom logic (sender exception) will be considered as the sender of the follow up e-mail, the other selected addresses will receive the follow up content in copy.

## Process 1: Sending Process

The history report will have the sending e-mail function available to handle all the output data. Once the function is selected, the program will pass all the information needed to the `ZMA0_CL_SFUT_EMAIL_SENDER`, so the reported data can be split and processed for the e-mail attachment creation.

The mentioned class will work with other local classes in order to organize and match the *SAP ABAP Clean* guidelines and recommendations, so it is possible to also see the implementation logic at the *Class-Relevant Local Types* tab on editors like *Eclipse* or the *SAP GUI* Editor itself.

Local classes used for the e-mail and attachment creation process (within `ZMA0_CL_SFUT_EMAIL_SENDER`):

* **LIF_SFUT_EMAIL**
Will handle all the types created for the usage within the sending e-mail process.

* **LCL_SFUT_EMAIL_DATABASE**
An instance with all the database access data to be injected during report execution.

* **LCL_SFUT_EMAIL_DETAILS**
An object with all the e-mail pertinent data, like subject, body and others.

* **LCL_SFUT_EMAIL**
Class which will trigger the e-mail sending process.

> **Note**
> Remember that this solution doesn't have any direct relation to the implementation of the logic in the history report, so all the changes that does not modify any method or class parameter wont impact the overall e-mail processing.

## Process 2: Attachment Creation

The attachment creation is done through `ZMA0_CL_SFUT_EMAIL_EXCEL` class which uses the [ABAP2XLSX](https://github.com/sapmentors/abap2xlsx) to create and handle the excel file, a basic example of its usage is shown below:

```
zmal_cl_sfut_email_excel=>factory( )->create_excel(
	EXPORTING
		delinq_table_raw =  " TYPE zmal_sfut_supplier_data_t
	IMPORTING
		ex_rawdata =        " TYPE solix_tab
		ex_bytecount =      " TYPE so_obj_len
).
```

The class will not do any sorting on the input table and will create a excel file with all of the fields that are registered on the input type `ZMA0_SFUT_SUPPLIER_DATA_T` even though some of them may be hidden in the resulting excel file they should not contain any sensitive information that belongs to anyone but the receiver.

#### Protected fields
All columns, except the ones related to the supplier's response, will be protected. Which means that it will not be editable by the user.

To do this first it's needed to protect the current worksheet using the method `protect_current_worksheet` after this, all cells in the current worksheet will be locked/protected. Now to unprotect them just assigning a style that has its members `protection->locked` set as bellow:

```
lo_style->protection->locked = zcl_excel_style_protection=>c_protection_unlocked
```

> **Note:** 
> Styles are not stackable, which means that you cannot set multiple styles to the same cell.*

#### Line colors
The line colors are defined by the shipping status (`CALC_SHIP_DATE`)

### Class Internal Operations
The class has two main methods, `create_params_sheet` and `create_forms_sheet` both are called in the `create_excel` method.

* `create_params_sheet`
This method creates a hidden worksheet with the ranges that will be used for the dropdowns in the `create_forms_sheet` method.

* `create_forms_sheet`
This method creates the worksheet with all the input table data that will be filled by the supplier.

## Process 3: Receiving Process

The receiving procedure will be available through the report program `ZMA0R_SFUT_EMAIL_RECEIVER` (Transaction ZM_SFUT_RECV).
The planner or buyer will have to insert the attachment file through the mentioned transaction selection screen. The program will use the class `ZMA0_CL_SFUT_EMAIL_RECEIVER` to retrieve the excel file and translate it into a internal table format.

The receiving process will use the classes ZCL_EXCEL_COMMON and ZCL_EXCEL_READER_2007 from the [ABAP2XLSX](https://github.com/sapmentors/abap2xlsx) library to retrieve values and convert them into an internal table just as previously mentioned.

The only information that needs to be informed to the e-mail receiver processor class is the file path on the users local machine.

The program will not partially process the informed spreadsheets, although the BAPI for schedule lines is called for each available register, the commit work procedure will only take place once all the information is properly validated and has no errors.
This approach was taken in order to provide the users information regarding all the data they are trying to upload regardless minor error the receiving procedure might found along the way. This would flexibilize 

#### Error Handling
The constructor process will use the exception class ZCX_EXCEL from the ABAP2XLSX library to enable the developers to handle problems that might occur during the instantiation procedure.


## Caveat & More Information

Due to the technological nature of the components assembled to create the Supplier Follow Up Tool E-mail, an Eclipse instance will be required in order to develop and maintain objects related to the implementation process.

> The **Eclipse 2020-12** version was used in the development phase of this project
>  You can download the latest version of the Eclipse available to work with SAP application through [this link](https://tools.hana.ondemand.com/#abap).

Considering the development procedure previously mentioned at the technical approach topic, please consider the mentioned caveats as relevant for future feature implementations and maintenance:

* Consider attach excel extraction and reading procedures on the clas `ZMA0_CL_SFUT_EMAIL_SENDER`;

* Use the `ZMA0_CL_SFUT_EMAIL_RECEIVER` to retrieve the excel from a local machine, and only to that purpose. This way we will be able to assure the file data extraction procedure will be loosely coupled enable the components reusability.

* All the methods and functions related to the EXCEL file creation will be attached to the class `ZMA0_CL_SFUT_EMAIL_EXCEL`;

* The `ZMA0_CL_SFUT_EMAIL_HELPER` class will be used on the maintenance view for the database table `ZMA0_SFUT_EMAIL` to handle the text component from the SO10 transaction.

* Update this documentation, keep it simple, clear and always befitting reality.

The John Deere naming standard document v1.6.8 was considered during the development phase of the initial commit of this project.
To check more information related to the John Deere coding guideline you can check [this link](http://share-internal.deere.com/teams/architecturepublications/L_Supporting%20Documents/SAP%20Materials%20-%20Summary%20Page/index.htm).
