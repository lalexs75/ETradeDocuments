{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit crpt;

{$warn 5023 off : no warning about unused units}
interface

uses
  crpt_cmp, km_cancellation, LP_base_types, MKIO_Order, reception_signs, 
  RemReq, retail_sale, shipment_signs, Vvod, vvod_contract_production, 
  Vvod_Imp, vvod_individuals, vvod_ostatky, LP_base_types_v3, shipment, 
  packcode_agregirovanie, packcode_transform, packcode_unagregirovan, 
  acceptance, withdrawal, cis_list, doc_list, CrptGlobalTypes, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('crpt_cmp', @crpt_cmp.Register);
end;

initialization
  RegisterPackage('crpt', @Register);
end.
