package com.luma.wadloader3.ddd3domain.files.services;

import java.nio.file.Path;
import java.util.List;

public interface FileZipper {
    Path zipFiles(List<Path> files);
}
