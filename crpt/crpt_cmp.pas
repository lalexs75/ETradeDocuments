unit crpt_cmp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TCRPTComponent = class(TComponent)
  private

  protected

  public

  published

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TradeEquipment',[TCRPTComponent]);
end;

end.
