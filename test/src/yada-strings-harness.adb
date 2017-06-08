with Yada.Strings.Suite;
with AUnit.Run;
with AUnit.Reporter.Text;

procedure Yada.Strings.Harness is
   procedure Run is new AUnit.Run.Test_Runner (Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);
end Yada.Strings.Harness;
