S <= ((not (((not A) and B) or ((not B) and A))) and Cin) or
      ((((not A) and B) or ((not B) and A)) and (not Cin));

Cout <= ((((not A) and B) or ((not B) and A)) and Cin)
          or (A and B);
