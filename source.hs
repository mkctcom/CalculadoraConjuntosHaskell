--Compile using: ghc -XPackageImports source.hs

module Main where
  
import Data.List
import Data.List.Split

import Data.Set (Set)
import qualified Data.Set as Set

import "gtk3" Graphics.UI.Gtk
import "gtk3" Graphics.UI.Gtk.Builder

import Control.Monad.Trans

main :: IO ()
main = do

	initGUI
	builder <- builderNew
	builderAddFromFile builder "gui_source.glade"
	window <- builderGetObject builder castToWindow "window1"
	uniaoButton <- builderGetObject builder castToButton "button1"
	interButton <- builderGetObject builder castToButton "button2"
	subButton <- builderGetObject builder castToButton "button3"
	minusButton <- builderGetObject builder castToButton "button4"
	compButton <- builderGetObject builder castToButton "button5"
	subpButton <- builderGetObject builder castToButton "button6"
	entry <- builderGetObject builder castToEntry "entry1"
	entry2 <- builderGetObject builder castToEntry "entry2"
	label <- builderGetObject builder castToLabel "label4"

	on uniaoButton buttonPressEvent $ tryEvent $ liftIO $ do
		saida1 <- getText entry
		saida2 <- getText entry2
		let list1 = splitOn "," saida1
		let list2 = splitOn "," saida2
		let set1 = Set.fromList(list1)
		let set2 = Set.fromList(list2)
		let unionSet = Set.union set1 set2
		let unionList = Set.toList unionSet
		let result = intercalate " " unionList
		labelSetText label ("União: { " ++ result ++ " }")

	on interButton buttonPressEvent $ tryEvent $ liftIO $ do
		saida1 <- getText entry
		saida2 <- getText entry2
		let list1 = splitOn "," saida1
		let list2 = splitOn "," saida2
		let set1 = Set.fromList(list1)
		let set2 = Set.fromList(list2)
		let interSet = Set.intersection set1 set2
		let interList = Set.toList interSet
		let result = intercalate " " interList
		labelSetText label ("Interseção: { " ++ result ++ " }")

	on subButton buttonPressEvent $ tryEvent $ liftIO $ do
		saida1 <- getText entry
		saida2 <- getText entry2
		if saida1 == "" && saida2 == ""
			then do
				labelSetText label ("É subconjunto? " ++ (boolToString True))
		else if saida1 == ""
			then do
				let list2 = splitOn "," saida2
				let set1 = Set.empty
				let set2 = Set.fromList(list2)
				let boolValue = Set.isSubsetOf set1 set2
				labelSetText label ("É subconjunto? " ++ (boolToString boolValue))
		else if saida2 == ""
			then do
				let list1 = splitOn "," saida1
				let set1 = Set.fromList(list1)				
				let set2 = Set.empty
				let boolValue = Set.isSubsetOf set1 set2
				labelSetText label ("É subconjunto? " ++ (boolToString boolValue))
		else
			do
				let list1 = splitOn "," saida1
				let list2 = splitOn "," saida2
				let set1 = Set.fromList(list1)
				let set2 = Set.fromList(list2)
				let boolValue = Set.isSubsetOf set1 set2
				labelSetText label ("É subconjunto? " ++ (boolToString boolValue))		

	on minusButton buttonPressEvent $ tryEvent $ liftIO $ do
		saida1 <- getText entry
		saida2 <- getText entry2
		let list1 = splitOn "," saida1
		let list2 = splitOn "," saida2
		let set1 = Set.fromList(list1)
		let set2 = Set.fromList(list2)
		let differSet = Set.difference set1 set2
		let differList = Set.toList differSet
		let result = intercalate " " differList
		labelSetText label ("Diferença: { " ++ result ++ " }")
	
	on compButton buttonPressEvent $ tryEvent $ liftIO $ do
		saida1 <- getText entry
		saida2 <- getText entry2
		
		if saida1 == "" && saida2 == ""
			then do
				labelSetText label ("Complemento: {  }")	
		else if saida1 == ""
			then do
				let list2 = splitOn "," saida2
				let set1 = Set.empty
				let set2 = Set.fromList(list2)
				let boolValue = Set.isSubsetOf set1 set2
				
				if boolValue 
					then do
						let compSet = Set.difference set2 set1
						let compList = Set.toList compSet
						let result = intercalate " " compList
						labelSetText label ("Complemento: { " ++ result ++ " }")
				else labelSetText label ("Complemento: {  }")

		else if saida2 == ""
			then do
				let list1 = splitOn "," saida1
				let set1 = Set.fromList(list1)				
				let set2 = Set.empty
				let boolValue = Set.isSubsetOf set1 set2
				
				if boolValue 
					then do
						let compSet = Set.difference set2 set1
						let compList = Set.toList compSet
						let result = intercalate " " compList
						labelSetText label ("Complemento: { " ++ result ++ " }")
				else labelSetText label ("Complemento: {  }")
		else
			do
				let list1 = splitOn "," saida1
				let list2 = splitOn "," saida2
				let set1 = Set.fromList(list1)
				let set2 = Set.fromList(list2)
				let boolValue = Set.isSubsetOf set1 set2
				
				if boolValue 
					then do
						let compSet = Set.difference set2 set1
						let compList = Set.toList compSet
						let result = intercalate " " compList
						labelSetText label ("Complemento: { " ++ result ++ " }")
				else labelSetText label ("Complemento: {  }")	

	on subpButton buttonPressEvent $ tryEvent $ liftIO $ do
		saida1 <- getText entry
		saida2 <- getText entry2
		if saida1 == "" && saida2 == ""
			then do
				labelSetText label ("É subconjunto? " ++ (boolToString False))
		else if saida1 == ""
			then do
				let list2 = splitOn "," saida2
				let set1 = Set.empty
				let set2 = Set.fromList(list2)
				let boolValue = Set.isProperSubsetOf set1 set2
				labelSetText label ("É subconjunto? " ++ (boolToString boolValue))
		else if saida2 == ""
			then do
				let list1 = splitOn "," saida1
				let set1 = Set.fromList(list1)				
				let set2 = Set.empty
				let boolValue = Set.isProperSubsetOf set1 set2
				labelSetText label ("É subconjunto? " ++ (boolToString boolValue))
		else
			do
				let list1 = splitOn "," saida1
				let list2 = splitOn "," saida2
				let set1 = Set.fromList(list1)
				let set2 = Set.fromList(list2)
				let boolValue = Set.isProperSubsetOf set1 set2
				labelSetText label ("É subconjunto? " ++ (boolToString boolValue))

	widgetShowAll window
 
	mainGUI

getText :: EntryClass self => self -> IO String
getText entry = do
	text <- entryGetText entry
	return text

boolToString :: Bool -> String
boolToString True = "SIM"
boolToString False = "NÃO"
