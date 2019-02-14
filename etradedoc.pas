unit ETradeDoc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TETradeDoc = class(TComponent)
  private

  protected

  public

  published

  end;

procedure Register;

implementation
uses InvoceExchangeFile;

procedure Register;
begin
  RegisterComponents('TradeEquipment',[TETradeDoc]);
end;

end.
