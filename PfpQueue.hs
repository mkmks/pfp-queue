{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<$>), optional)
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (pack, unpack)
import Data.Time
import System.Directory
import System.Process
import Text.CSV (parseCSVFromFile)
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


home :: FilePath
home = "/home/pfp-queue/"

students :: FilePath
students = home ++ "students.csv"

archiveDir :: FilePath
archiveDir = home ++ "archive/"

incomingDir :: FilePath
incomingDir = home ++ "incoming/"

config = Just defaultServerConfig
  {
  port = 80
  }

main :: IO ()
main = serve Nothing myApp

myApp :: ServerPart Response
myApp = msum
        [ 
         upload
        ]

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.title (toHtml title)
    H.body $ do
      body
      p $ a ! href "/" $ "back home"

upload :: ServerPart Response
upload =
       msum [ uploadForm
            , handleUpload
            ]
    where
    uploadForm :: ServerPart Response
    uploadForm =
             do method GET
                ok $ template "Parallel Functional Programming benchmarking queue" $ do
                  H.p "Here you submit code to be benchmarked. If you're new here, read what's beneath."
                  form ! enctype "multipart/form-data" ! A.method "POST" ! action "/" $ do
                    label ! A.for "email" $ "Email address: "
                    input ! type_ "text" ! A.id "email" ! name "email"
                    H.br
                    label ! A.for "pn" $ "Personnummer: "
                    input ! type_ "text" ! A.id "pn" ! name "pn"
                    "(ten digits and a dash)"
                    H.br
                    label ! A.for "hs" $ "Haskell"
                    input ! type_ "radio" ! A.id "hs"   ! name "file_type" ! value "hs"
                    label ! A.for "hs" $ "CUDA"
                    input ! type_ "radio" ! A.id "cuda" ! name "file_type" ! value "cuda"
                    H.br
                    input ! type_ "file" ! name "file_upload" ! size "40"
                    input ! type_ "submit" ! value "upload"
                  H.h2 "README"
                  H.p "This is an interface to a batch processing system that lets many students to share a massively parallel Amazon EC2 instance. It is not your personal development machine. Submit code here only when you want to run final benchmarks before you submit your lab assignment. The instance will only run jobs every two hours. This period might be increased later."
                  H.p "If you want to benchmark a Haskell program on a multicore machine, your submission should be a tarball (.tar.gz) containing Haskell code. You may supply input data in the same tarball. The source code should have a Main.hs file defining a 'main' function. We compile it as follows:"
                  H.blockquote "ghc -O2 -threaded -eventlog -rtsopts --make Main.hs"
                  H.p "If you want to benchmark a CUDA program, submit a single .cu file. The name is not important. Note, tarballs won't be processed at this point, so include all input data in that file. This is because we're still deciding about a good compromise between permissiveness and security of the batch system. For the same reason, we can't allow custom build scripts yet. The compiler is invoked as follows:"
                  H.blockquote "nvcc file.c -o file"
                  H.p "Compiler output, runtime output and an eventlog will be mailed to the provided address. The submitted code stays archived, in case we want to investigate potential issues."
                  H.p "The compiled program is run under certain restrictions. Network access is blocked. If produced output exceeds 0.5 Mb, no output will be mailed. If your code doesn't terminate within 15 seconds, we'll do the favor, no questions asked. There is no way to supply command line arguments; this is done on purpose."
                  H.p "You agree not to use these machines for any illicit purposes. The rules are the same as for any other Chalmers machine. Remember, if you're not caught immediately, it doesn't mean you won't be caught later."
                  H.p $ do
                    "If you have any questions or remarks about this service, or you think that your job was lost in the queue, contact the course assistant Nikita Frolov "
                    a ! href "mailto:frolov@chalmers.se" $ "<frolov@chalmers.se>"
                    "."

    handleUpload :: ServerPart Response
    handleUpload =
        do email <- lookText "email"
           pn <- lookText "pn"
           file_type <- lookText "file_type"
           (tmpFile, uploadName, contentType) <- lookFile "file_upload"
           authdata <- liftIO $ parseCSVFromFile students
           either (\_ -> confError)
                 (\r -> do
                     let r' = map (\z -> case z of { (x:y:[]) -> (x,y) ; _ -> ("","") }) r
                     case lookup (unpack email) r' of
                       Just x -> if x == unpack pn
                                 then
                                   do
                                     t <- liftIO getCurrentTime
                                     z <- liftIO getCurrentTimeZone
                                     let jobtime = utcToLocalTime z t
                                         jobfile = unpack file_type ++ "+" ++ unpack email ++ "+" ++ unpack pn ++ "+" ++ show jobtime ++ "+.tar.gz"
                                     liftIO $ copyFile tmpFile $ archiveDir ++ jobfile
                                     liftIO $ copyFile tmpFile $ incomingDir ++ jobfile
                                     liftIO $ system $ "chmod 666 " ++ incomingDir ++ "*"
                                     ok $ template "File uploaded" $ do
                                       H.p $ do
                                         toHtml $ "The job was put into queue at " ++ show jobtime ++ ". Logs will be mailed to "
                                         a ! (href $ "mailto:") $ toHtml $ "<" ++ unpack email ++ ">"
                                         "."
                                 else authFailure
                       Nothing -> authFailure2) authdata

    

    confError :: ServerPart Response
    confError = do
      ok $ template "Error" $ do
        p "There was an issue with the server confuguration."
            
    authFailure :: ServerPart Response
    authFailure = do
      ok $ template "Authentication failed" $ do
        p "The details you've entered do not correspond to course registration data."

    authFailure2 :: ServerPart Response
    authFailure2 = do
      ok $ template "Authentication failed" $ do
        p "Check if you've been registered for the course."
