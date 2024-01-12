def gprint(s: String): Void = {
    write(27, true); // green
    write("[0;32m");  
    write(s);
    write(27, true); // reset
    write("[0m");
}

{
    write("こんにちは世界！\n");
    write("here are some crabs: 🦀 🦀 🦀\n");
    gprint("thank you!\n");
}