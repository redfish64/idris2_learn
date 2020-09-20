libc : String -> String
libc fn = "C:" ++ fn ++ ",libc"

%foreign (libc "puts")
puts : String -> PrimIO Int

main : IO Int
main = primIO $ puts "Hello cworld!"
