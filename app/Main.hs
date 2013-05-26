import Prelude              (IO)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Ecom.Settings             (parseExtra)
import Ecom.Application          (makeApplication)

main :: IO ()
main = defaultMain (fromArgs parseExtra) makeApplication
