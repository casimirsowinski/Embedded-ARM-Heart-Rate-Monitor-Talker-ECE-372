# Embedded-ARM-Heart-Rate-Monitor-Talker-ECE-372

For documentation and a full write-up, visit my site: www.casimirsowinski.wordpress.com

This program will give vocal feedback to a user wearing a heart-rate monitor and is written in ARM assembly for a BeagleBone Black. 

The program gets an input from a heart beat detector (can be simulated by pressing a button), calculates running heart rate, and sends an ASCII message to a RC8660 talker board over UART that synthesizes speech to say the current heartrate. 
